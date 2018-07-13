module Kit.CodeGen.C.CExpr where

import Numeric
import Language.C
import Language.C.Data.Position
import Kit.Ast
import Kit.Ir
import Kit.Parser.Span
import Kit.Str

u x = x undefNode
cpos p x = x $ case (file p) of
  Just f -> mkNodeInfoOnlyPos
    $ position 0 (s_unpack f) (start_line p) (start_col p) Nothing
  Nothing -> undefNode

ctype :: BasicType -> [CTypeSpec]
ctype BasicTypeVoid       = [u CVoidType]
ctype BasicTypeBool       = [u CBoolType]
ctype (BasicTypeInt   8 ) = [u CSignedType, u CCharType]
ctype (BasicTypeInt   16) = [u CSignedType, u CShortType]
ctype (BasicTypeInt   32) = [u CSignedType, u CLongType]
ctype (BasicTypeInt   64) = [u CSignedType, u CLongType, u CLongType]
ctype (BasicTypeUint  8 ) = [u CUnsigType, u CCharType]
ctype (BasicTypeUint  16) = [u CUnsigType, u CShortType]
ctype (BasicTypeUint  32) = [u CUnsigType, u CLongType]
ctype (BasicTypeUint  64) = [u CUnsigType, u CLongType, u CLongType]
ctype (BasicTypeFloat 32) = [u CFloatType]
ctype (BasicTypeFloat 64) = [u CDoubleType]
ctype (BasicTypeAtom  _ ) = [u CUnsigType, u CLongType]
ctype (BasicTypeStruct (name, _)) =
  [ u $ CSUType $ u $ CStruct CStructTag
                              (Just $ internalIdent $ s_unpack name)
                              Nothing
                              []
  ]
ctype (BasicTypeSimpleEnum name _) =
  [u $ CEnumType $ u $ CEnum (Just (internalIdent $ s_unpack name)) Nothing []]
ctype (BasicTypeComplexEnum name _) = ctype (BasicTypeStruct (name, []))
-- TODO: CArr
-- TODO: CPtr

ctype' = map CTypeSpec . ctype

--transpile :: [Expr] -> [CStat]
--transpile exprs

transpileExpr :: IrExpr -> CExpr
transpileExpr (IrIdentifier s) = u $ CVar $ internalIdent $ s_unpack s
transpileExpr (IrLiteral (BoolValue b)) =
  CConst $ u $ CIntConst $ cInteger (if b then 1 else 0)
transpileExpr (IrLiteral (IntValue i)) =
  CConst $ u $ CIntConst $ transpileInt (s_unpack i)
transpileExpr (IrLiteral (FloatValue f)) =
  CConst $ u $ CFloatConst $ transpileFloat (s_unpack f)
transpileExpr (IrLiteral (StringValue s)) =
  CConst $ u $ CStrConst $ cString $ s_unpack s
transpileExpr (IrBinop Assign e1 e2) =
  u $ CAssign (CAssignOp) (transpileExpr e1) (transpileExpr e2)
transpileExpr (IrBinop op e1 e2) =
  u $ CBinary (transpileBinop op) (transpileExpr e1) (transpileExpr e2)
transpileExpr (IrPreUnop op e1) =
  u $ CUnary (transpilePreUnop op) (transpileExpr e1)
transpileExpr (IrPostUnop op e1) =
  u $ CUnary (transpilePostUnop op) (transpileExpr e1)
transpileExpr (IrField e s) =
  u $ CMember (transpileExpr e) (internalIdent $ s_unpack s) False
transpileExpr (IrArrayAccess e1 e2) =
  u $ CIndex (transpileExpr e1) (transpileExpr e2)
transpileExpr (IrCall e args) =
  u $ CCall (transpileExpr e) [ transpileExpr x | x <- args ]
transpileExpr (IrCast e t) =
  u $ CCast (u $ CDecl (ctype' t) []) (transpileExpr e)
--transpileExpr (Expr {expr = VectorLiteral e}) = u $ CA

transpileStmt :: IrExpr -> CStat
transpileStmt IrBreak             = u CBreak
transpileStmt IrContinue          = u CCont
transpileStmt (IrReturn (Just r)) = u $ CReturn $ Just $ transpileExpr r
transpileStmt (IrReturn Nothing ) = u $ CReturn Nothing
transpileStmt (IrBlock e) = u $ CCompound [] [ transpileBlockItem x | x <- e ]
transpileStmt (IrIf cond e1 (Just e2)) =
  u $ CIf (transpileExpr cond) (transpileStmt e1) (Just $ transpileStmt e2)
transpileStmt (IrIf cond e1 Nothing) =
  u $ CIf (transpileExpr cond) (transpileStmt e1) (Nothing)
transpileStmt (IrWhile cond e) =
  u $ CWhile (transpileExpr cond) (transpileStmt e) False
transpileStmt (IrFor v id_type start end body) = u $ CFor
  (Right $ u $ CDecl
    (ctype' id_type)
    [ ( Just $ var_to_cdeclr v
      , Just $ u $ CInitExpr $ transpileExpr start
      , Nothing
      )
    ]
  )
  (Just $ transpileExpr (IrBinop Lt (IrIdentifier v) (end)))
  (Just $ transpileExpr (IrPreUnop Inc (IrIdentifier v)))
  (transpileStmt body)
transpileStmt e = u $ CExpr $ Just $ transpileExpr e

var_to_cdeclr x =
  u $ CDeclr (Just $ internalIdent $ s_unpack x) [] (Nothing) []

transpileBlockItem :: IrExpr -> CBlockItem
transpileBlockItem (IrVarDeclaration v t var_default) = CBlockDecl $ u $ CDecl
  (ctype' t)
  [(Just vn, body, Nothing)]
 where
  vn   = var_to_cdeclr $ v
  body = case var_default of
    Just x  -> Just $ u $ CInitExpr $ transpileExpr x
    Nothing -> Nothing
transpileBlockItem x = CBlockStmt $ transpileStmt x

-- TODO: validate
r1 x = fst (x !! 0)

transpileInt ('0' : 'x' : s) = cInteger $ r1 $ readHex s
transpileInt ('0' : 'b' : s) = cInteger $ r1 $ readInt 2 isBin readBin s
 where
  isBin x = (x == '0' || x == '1')
  readBin '0' = 0
  readBin '1' = 1
transpileInt ('0' : 'o' : s) = cInteger $ r1 $ readOct ('0' : s)
transpileInt s               = cInteger $ r1 $ readDec s

transpileFloat s = cFloat $ r1 $ readFloat s

transpileBinop Add        = CAddOp
transpileBinop Sub        = CSubOp
transpileBinop Mul        = CMulOp
transpileBinop Div        = CDivOp
transpileBinop Mod        = CRmdOp
transpileBinop Eq         = CEqOp
transpileBinop Neq        = CNeqOp
transpileBinop Gt         = CGrOp
transpileBinop Lt         = CLeOp
transpileBinop Gte        = CGeqOp
transpileBinop Lte        = CLeqOp
transpileBinop LeftShift  = CShlOp
transpileBinop RightShift = CShrOp
transpileBinop And        = CLndOp
transpileBinop Or         = CLorOp
transpileBinop BitAnd     = CAndOp
transpileBinop BitOr      = COrOp
transpileBinop BitXor     = CXorOp

transpilePreUnop Inc        = CPreIncOp
transpilePreUnop Dec        = CPreDecOp
transpilePreUnop Sub        = CMinOp
transpilePreUnop Invert     = CNegOp
transpilePreUnop InvertBits = CCompOp

transpilePostUnop Inc = CPostIncOp
transpilePostUnop Dec = CPostDecOp
