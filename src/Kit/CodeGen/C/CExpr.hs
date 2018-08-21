{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Kit.CodeGen.C.CExpr where

import Data.List
import Numeric
import Language.C
import Language.C.Data.Position
import Kit.Ast
import Kit.Ir
import Kit.Parser.Span
import Kit.Str

cDecl :: BasicType -> Maybe Str -> Maybe (CInitializer NodeInfo) -> CDecl
cDecl t ident body = u
  $ CDecl (map CTypeSpec typeSpec) [(Just cdeclr, body, Nothing)]
 where
  (typeSpec, derivedDeclr) = ctype t
  cdeclr                   = u $ CDeclr
    (case ident of
      Just x  -> Just $ internalIdent $ s_unpack x
      Nothing -> Nothing
    )
    derivedDeclr
    Nothing
    []

u x = x undefNode
cpos p x =
  x $ mkNodeInfoOnlyPos $ position 0 (file p) (startLine p) (startCol p) Nothing

ctype :: BasicType -> ([CTypeSpec], [CDerivedDeclr])
ctype BasicTypeVoid = ([u CVoidType], [])
ctype BasicTypeBool = ([u CBoolType], [])
ctype (BasicTypeInt n) =
  ([u $ CTypeDef (internalIdent $ "int" ++ show n ++ "_t")], [])
ctype (BasicTypeUint n) =
  ([u $ CTypeDef (internalIdent $ "uint" ++ show n ++ "_t")], [])
ctype (BasicTypeFloat 32) = ([u CFloatType], [])
ctype (BasicTypeFloat 64) = ([u CDoubleType], [])
ctype (BasicTypeAtom    ) = ([u CUnsigType, u CLongType], [])
ctype (BasicTypeStruct name args) =
  ( [ u $ CSUType $ u $ CStruct
        CStructTag
        (case name of
          Just name -> Just $ internalIdent $ s_unpack name
          Nothing   -> Nothing
        )
        (case name of
          Just name -> Nothing
          Nothing -> -- anonymous
            Just
              [ cDecl argType (Just argName) Nothing
              | (argName, argType) <- args
              ]
        )
        []
    ]
  , []
  )
ctype (BasicTypeUnion name args) =
  ( [ u $ CSUType $ u $ CStruct
        CUnionTag
        (case name of
          Just name -> Just $ internalIdent $ s_unpack name
          Nothing   -> Nothing
        )
        (case name of
          Just name -> Nothing
          Nothing -> -- anonymous
            Just
              [ cDecl argType (Just argName) Nothing
              | (argName, argType) <- args
              ]
        )
        []
    ]
  , []
  )
ctype (BasicTypeSimpleEnum name _) =
  ( [ u $ CEnumType $ u $ CEnum
        (case name of
          Just name -> Just (internalIdent $ s_unpack name)
          Nothing   -> Nothing
        )
        Nothing
        []
    ]
  , []
  )
ctype (BasicTypeComplexEnum name _) = ctype (BasicTypeStruct (Just name) [])
ctype (BasicTypeTuple name t) =
  ( [ u $ CSUType $ u $ CStruct CStructTag
                                (Just (internalIdent $ s_unpack name))
                                Nothing
                                []
    ]
  , []
  )
ctype (CPtr x) = (fst t, (u $ CPtrDeclr []) : snd t) where t = ctype x
ctype (BasicTypeFunction rt args var) =
  let (rta, rtb) = ctype rt
  in  ( rta
      , [ u $ CFunDeclr
            (Right ([ cDecl t Nothing Nothing | (name, t) <- args ], var))
            []
        ]
        ++ rtb
      )
-- TODO: CArray
ctype (CArray _ _      ) = undefined
ctype (BasicTypeUnknown) = undefined

intFlags f = foldr (\f acc -> setFlag f acc) noFlags f

transpileExpr :: IrExpr -> CExpr
transpileExpr (IrIdentifier s) = u $ CVar $ internalIdent $ s_unpack s
transpileExpr (IrLiteral (BoolValue b)) =
  CConst $ u $ CIntConst $ cInteger (if b then 1 else 0)
transpileExpr (IrLiteral (IntValue i t@(BasicTypeFloat _))) =
  transpileExpr (IrLiteral (FloatValue (s_pack $ show i) t))
transpileExpr (IrLiteral (IntValue i t)) = CConst $ u $ CIntConst $ CInteger
  (toInteger i)
  DecRepr
  (intFlags
    (case t of
      BasicTypeInt  32 -> [FlagLong]
      BasicTypeInt  64 -> [FlagLongLong]
      BasicTypeUint 32 -> [FlagUnsigned, FlagLong]
      BasicTypeUint 64 -> [FlagUnsigned, FlagLongLong]
      _                -> []
    )
  )
transpileExpr (IrLiteral (FloatValue f t)) =
  CConst $ u $ CFloatConst $ transpileFloat (s_unpack f)
transpileExpr (IrLiteral (StringValue s)) =
  u $ CCast (cDecl (CPtr $ BasicTypeInt 8) Nothing Nothing) $ CConst $ u $ CStrConst $ cString $ s_unpack s
transpileExpr (IrBinop Assign e1 e2) =
  u $ CAssign (CAssignOp) (transpileExpr e1) (transpileExpr e2)
transpileExpr (IrBinop (AssignOp op) e1 e2) =
  u $ CAssign (transpileAssignop op) (transpileExpr e1) (transpileExpr e2)
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
  u $ CCast (cDecl t Nothing Nothing) (transpileExpr e)
--transpileExpr (Expr {expr = VectorLiteral e}) = u $ CA
transpileExpr (IrStructInit t fields) = u $ CCompoundLit
  (cDecl t Nothing Nothing)
  [ ( [u $ CMemberDesig (internalIdent $ s_unpack name)]
    , u $ CInitExpr (transpileExpr e)
    )
  | (name, e) <- fields
  ]
transpileExpr (IrEnumInit (BasicTypeSimpleEnum _ _) discriminant []) =
  transpileExpr (IrIdentifier discriminant)
transpileExpr (IrEnumInit t discriminant []) = u $ CCompoundLit
  (cDecl t Nothing Nothing)
  [ ( [u $ CMemberDesig (internalIdent $ s_unpack discriminantFieldName)]
    , u $ CInitExpr $ transpileExpr (IrIdentifier discriminant)
    )
  ]
transpileExpr (IrEnumInit t@(BasicTypeComplexEnum name variants) discriminant fields)
  = u $ CCompoundLit
    (cDecl t Nothing Nothing)
    [ ( [u $ CMemberDesig (internalIdent $ s_unpack discriminantFieldName)]
      , u $ CInitExpr $ transpileExpr (IrIdentifier discriminant)
      )
    , ( [ u
          $  CMemberDesig
          $  internalIdent
          $  "__variant.variant_"
          ++ (s_unpack discriminant)
        ]
      , u $ CInitExpr $ transpileExpr
        (IrStructInit
          (BasicTypeStruct (Just $ s_concat [name, "_Variant_", discriminant])
                           []
          )
          (zip (map fst $ getVariantFieldNames variants discriminant) fields)
        )
      )
    ]
transpileExpr (IrTupleInit t vals) = u $ CCompoundLit
  (cDecl t Nothing Nothing)
  [ ( [u $ CMemberDesig (internalIdent $ "__slot" ++ (show i))]
    , u $ CInitExpr (transpileExpr e)
    )
  | (i, e) <- zip [0 ..] vals
  ]
transpileExpr (IrSizeOf t) = u $ CSizeofType (cDecl t Nothing Nothing)

getVariantFieldNames variants discriminant =
  case find (\(name, _) -> name == discriminant) variants of
    Just (_, args) -> args
    _              -> []

transpileStmt :: IrExpr -> CStat
transpileStmt IrBreak               = u CBreak
transpileStmt IrContinue            = u CCont
transpileStmt (IrReturn   (Just r)) = u $ CReturn $ Just $ transpileExpr r
transpileStmt (IrReturn   Nothing ) = u $ CReturn Nothing
transpileStmt (IrBlock e) = u $ CCompound [] [ transpileBlockItem x | x <- e ]
transpileStmt (IrCompound e       ) = transpileStmt (IrBlock e)
transpileStmt (IrIf cond e1 (Just e2)) =
  u $ CIf (transpileExpr cond) (transpileStmt e1) (Just $ transpileStmt e2)
transpileStmt (IrIf cond e1 Nothing) =
  u $ CIf (transpileExpr cond) (transpileStmt e1) (Nothing)
transpileStmt (IrWhile cond e d) =
  u $ CWhile (transpileExpr cond) (transpileStmt e) d
transpileStmt (IrFor v idType start end body) = u $ CFor
  (Right $ cDecl idType (Just v) (Just $ u $ CInitExpr $ transpileExpr start))
  (Just $ transpileExpr (IrBinop Lt (IrIdentifier v) (end)))
  (Just $ transpileExpr (IrPreUnop Inc (IrIdentifier v)))
  (transpileStmt body)
transpileStmt (IrSwitch val cases def) = u $ CSwitch
  (transpileExpr val)
  (u $ CCompound
    []
    (  (foldr
         (++)
         []
         [ [ CBlockStmt $ u $ CCase (transpileExpr val) (transpileStmt body)
           , transpileBlockItem IrBreak
           ]
         | (val, body) <- cases
         ]
       )
    ++ (case def of
         Just x ->
           [ CBlockStmt
               $ u
               $ CDefault
                   (u $ CCompound
                     []
                     [transpileBlockItem x, transpileBlockItem IrBreak]
                   )
           ]
         Nothing -> []
       )
    )
  )
transpileStmt e = u $ CExpr $ Just $ transpileExpr e

var_to_cdeclr x =
  u $ CDeclr (Just $ internalIdent $ s_unpack x) [] (Nothing) []

transpileBlockItem :: IrExpr -> CBlockItem
transpileBlockItem (IrVarDeclaration v t varDefault) = CBlockDecl
  $ cDecl t (Just v) body
 where
  body = case varDefault of
    Just x  -> Just $ u $ CInitExpr $ transpileExpr x
    Nothing -> Nothing
transpileBlockItem x = CBlockStmt $ transpileStmt x

-- TODO: validate
r1 x = fst $ head x

transpileInt ('0' : 'x' : s) = cInteger $ r1 $ readHex s
transpileInt ('0' : 'b' : s) = cInteger $ r1 $ readInt 2 isBin readBin s
 where
  isBin x = (x == '0' || x == '1')
  readBin '0' = 0
  readBin '1' = 1
transpileInt ('0' : 'o' : s) = cInteger $ r1 $ readOct ('0' : s)
transpileInt s               = cInteger $ r1 $ readDec s

transpileFloat s = CFloat s

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
transpileBinop _          = undefined

transpileAssignop Add        = CAddAssOp
transpileAssignop Sub        = CSubAssOp
transpileAssignop Mul        = CMulAssOp
transpileAssignop Div        = CDivAssOp
transpileAssignop Mod        = CRmdAssOp
transpileAssignop BitAnd     = CAndAssOp
transpileAssignop BitOr      = COrAssOp
transpileAssignop BitXor     = CXorAssOp
transpileAssignop LeftShift  = CShlAssOp
transpileAssignop RightShift = CShrAssOp
transpileAssignop _          = undefined

transpilePreUnop Inc        = CPreIncOp
transpilePreUnop Dec        = CPreDecOp
transpilePreUnop Sub        = CMinOp
transpilePreUnop Invert     = CNegOp
transpilePreUnop InvertBits = CCompOp
transpilePreUnop Ref        = CAdrOp
transpilePreUnop Deref      = CIndOp
transpilePreUnop _          = undefined

transpilePostUnop Inc = CPostIncOp
transpilePostUnop Dec = CPostDecOp
transpilePostUnop _   = undefined
