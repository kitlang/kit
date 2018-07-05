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
    Just f -> mkNodeInfoOnlyPos $ position 0 (s_unpack f) (start_line p) (start_col p) Nothing
    Nothing -> undefNode

  ctype :: BasicType -> [CTypeSpec]
  ctype BasicTypeVoid = [u CVoidType]
  ctype BasicTypeBool = [u CBoolType]
  ctype (BasicTypeInt 8) = [u CSignedType, u CCharType]
  ctype (BasicTypeInt 16) = [u CSignedType, u CShortType]
  ctype (BasicTypeInt 32) = [u CSignedType, u CLongType]
  ctype (BasicTypeInt 64) = [u CSignedType, u CLongType, u CLongType]
  ctype (BasicTypeUint 8) = [u CUnsigType, u CCharType]
  ctype (BasicTypeUint 16) = [u CUnsigType, u CShortType]
  ctype (BasicTypeUint 32) = [u CUnsigType, u CLongType]
  ctype (BasicTypeUint 64) = [u CUnsigType, u CLongType, u CLongType]
  ctype (BasicTypeFloat 32) = [u CFloatType]
  ctype (BasicTypeFloat 64) = [u CDoubleType]
  ctype (BasicTypeAtom _) = [u CUnsigType, u CLongType]
  ctype (BasicTypeStruct (name, _)) = [u $ CSUType $ u $ CStruct CStructTag (Just $ internalIdent $ s_unpack name) Nothing []]
  ctype (BasicTypeSimpleEnum name _) = [u $ CEnumType $ u $ CEnum (Just (internalIdent $ s_unpack name)) Nothing []]
  ctype (BasicTypeComplexEnum name _) = ctype (BasicTypeStruct (name, []))

  ctype' = map CTypeSpec . ctype

  --transpile :: [Expr] -> [CStat]
  --transpile exprs

  transpile_expr :: IrExpr -> CExpr
  transpile_expr (IrIdentifier s) = u $ CVar $ internalIdent $ s_unpack s
  transpile_expr (IrLiteral (BoolValue b)) = CConst $ u $ CIntConst $ cInteger (if b then 1 else 0)
  transpile_expr (IrLiteral (IntValue i)) = CConst $ u $ CIntConst $ transpile_int (s_unpack i)
  transpile_expr (IrLiteral (FloatValue f)) = CConst $ u $ CFloatConst $ transpile_float (s_unpack f)
  transpile_expr (IrBinop op e1 e2) = u $ CBinary (transpile_binop op) (transpile_expr e1) (transpile_expr e2)
  transpile_expr (IrPreUnop op e1) = u $ CUnary (transpile_pre_unop op) (transpile_expr e1)
  transpile_expr (IrPostUnop op e1) = u $ CUnary (transpile_post_unop op) (transpile_expr e1)
  transpile_expr (IrField e s) = u $ CMember (transpile_expr e) (internalIdent $ s_unpack s) False
  transpile_expr (IrArrayAccess e1 e2) = u $ CIndex (transpile_expr e1) (transpile_expr e2)
  transpile_expr (IrCall e args) = u $ CCall (transpile_expr e) [transpile_expr x | x <- args]
  transpile_expr (IrCast e t) = u $ CCast (u $ CDecl (ctype' t) []) (transpile_expr e)
  --transpile_expr (Expr {expr = VectorLiteral e}) = u $ CA

  transpile_stmt :: IrExpr -> CStat
  transpile_stmt IrBreak = u CBreak
  transpile_stmt IrContinue = u CCont
  transpile_stmt (IrReturn (Just r)) = u $ CReturn $ Just $ transpile_expr r
  transpile_stmt (IrReturn Nothing) = u $ CReturn Nothing
  transpile_stmt (IrBlock e) = u $ CCompound [] [transpile_block_item x | x <- e]
  transpile_stmt (IrIf cond e1 (Just e2)) = u $ CIf (transpile_expr cond) (transpile_stmt e1) (Just $ transpile_stmt e2)
  transpile_stmt (IrIf cond e1 Nothing) = u $ CIf (transpile_expr cond) (transpile_stmt e1) (Nothing)
  transpile_stmt (IrWhile cond e) = u $ CWhile (transpile_expr cond) (transpile_stmt e) False
  transpile_stmt (IrFor v id_type start end body) =
    u $ CFor (Right $ u $ CDecl (ctype' id_type) [(Just $ var_to_cdeclr v, Just $ u $ CInitExpr $ transpile_expr start, Nothing)]) (Just $ transpile_expr (IrBinop Lt (IrIdentifier v) (end))) (Just $ transpile_expr (IrPreUnop Inc (IrIdentifier v))) (transpile_stmt body)
  transpile_stmt e = u $ CExpr $ Just $ transpile_expr e

  var_to_cdeclr x = u $ CDeclr (Just $ internalIdent $ s_unpack x) [] (Nothing) []

  transpile_block_item :: IrExpr -> CBlockItem
  transpile_block_item (IrVarDeclaration v t var_default) =
    CBlockDecl $ u $ CDecl (ctype' t) [(Just vn, body, Nothing)]
    where vn = var_to_cdeclr $ v
          body = case var_default of
                   Just x -> Just $ u $ CInitExpr $ transpile_expr x
                   Nothing -> Nothing
  transpile_block_item x = CBlockStmt $ transpile_stmt x

  -- TODO: validate
  r1 x = fst (x !! 0)

  transpile_int ('0':'x':s) = cInteger $ r1 $ readHex s
  transpile_int ('0':'b':s) = cInteger $ r1 $ readInt 2 isBin readBin s
                              where isBin x = (x == '0' || x == '1')
                                    readBin '0' = 0
                                    readBin '1' = 1
  transpile_int ('0':'o':s) = cInteger $ r1 $ readOct ('0':s)
  transpile_int s = cInteger $ r1 $ readDec s

  transpile_float s = cFloat $ r1 $ readFloat s

  transpile_binop Add = CAddOp
  transpile_binop Sub = CSubOp
  transpile_binop Mul = CMulOp
  transpile_binop Div = CDivOp
  transpile_binop Mod = CRmdOp
  transpile_binop Eq = CEqOp
  transpile_binop Neq = CNeqOp
  transpile_binop Gt = CGrOp
  transpile_binop Lt = CLeOp
  transpile_binop Gte = CGeqOp
  transpile_binop Lte = CLeqOp
  transpile_binop LeftShift = CShlOp
  transpile_binop RightShift = CShrOp
  transpile_binop And = CLndOp
  transpile_binop Or = CLorOp
  transpile_binop BitAnd = CAndOp
  transpile_binop BitOr = COrOp
  transpile_binop BitXor = CXorOp

  transpile_pre_unop Inc = CPreIncOp
  transpile_pre_unop Dec = CPreDecOp
  transpile_pre_unop Sub = CMinOp
  transpile_pre_unop Invert = CNegOp
  transpile_pre_unop InvertBits = CCompOp

  transpile_post_unop Inc = CPostIncOp
  transpile_post_unop Dec = CPostDecOp
