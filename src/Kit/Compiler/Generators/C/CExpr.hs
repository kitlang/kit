{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Kit.Compiler.Generators.C.CExpr (
  cDecl,
  ctype,
  cIdent,
  u,
  transpileExpr,
  transpileStmt,
  initializerExpr
) where

import Data.List
import Numeric
import Language.C
import Language.C.Data.Position
import Kit.Ast
import Kit.Error
import Kit.NameMangling
import Kit.Ir
import Kit.Ast.Span
import Kit.Str

reservedWord :: String -> Bool
reservedWord "auto"     = True
reservedWord "break"    = True
reservedWord "case"     = True
reservedWord "char"     = True
reservedWord "const"    = True
reservedWord "continue" = True
reservedWord "default"  = True
reservedWord "do"       = True
reservedWord "int"      = True
reservedWord "long"     = True
reservedWord "register" = True
reservedWord "return"   = True
reservedWord "short"    = True
reservedWord "signed"   = True
reservedWord "sizeof"   = True
reservedWord "static"   = True
reservedWord "struct"   = True
reservedWord "switch"   = True
reservedWord "typedef"  = True
reservedWord "union"    = True
reservedWord "unsigned" = True
reservedWord "void"     = True
reservedWord "volatile" = True
reservedWord "while"    = True
reservedWord "double"   = True
reservedWord "else"     = True
reservedWord "enum"     = True
reservedWord "extern"   = True
reservedWord "float"    = True
reservedWord "for"      = True
reservedWord "goto"     = True
reservedWord "if"       = True
reservedWord _          = False

cIdent x =
  internalIdent
    $ let s = s_unpack x in if reservedWord s then "__kit__" ++ s else s

cDecl :: BasicType -> Maybe TypePath -> Maybe (CInitializer NodeInfo) -> CDecl
cDecl t ident body = u $ CDecl typeSpec [(Just cdeclr, body, Nothing)]
 where
  (typeSpec, derivedDeclr) = ctype t
  cdeclr                   = u $ CDeclr
    (case ident of
      Just x  -> Just $ cIdent $ mangleName x
      Nothing -> Nothing
    )
    derivedDeclr
    Nothing
    []

u x = x undefNode
cpos p x = x $ mkNodeInfoOnlyPos $ Language.C.Data.Position.position
  0
  (file p)
  (startLine p)
  (startCol p)
  Nothing

ctype :: BasicType -> ([CDeclSpec], [CDerivedDeclr])
ctype (BasicTypeConst t) =
  let (a, b) = ctype t in ((CTypeQual $ u CConstQual) : a, b)
ctype BasicTypeVoid    = ([CTypeSpec $ u CVoidType], [])
ctype BasicTypeBool    = ([CTypeSpec $ u CBoolType], [])
ctype (BasicTypeCChar) = ([CTypeSpec $ u CCharType], [])
ctype (BasicTypeCInt ) = ([CTypeSpec $ u CIntType], [])
ctype (BasicTypeCUint) =
  ([CTypeSpec $ u $ CTypeDef (internalIdent "unsigned int")], [])
ctype (BasicTypeCSize) =
  ([CTypeSpec $ u $ CTypeDef (internalIdent "size_t")], [])
ctype (BasicTypeInt n) =
  ([CTypeSpec $ u $ CTypeDef (internalIdent $ "int" ++ show n ++ "_t")], [])
ctype (BasicTypeUint n) =
  ([CTypeSpec $ u $ CTypeDef (internalIdent $ "uint" ++ show n ++ "_t")], [])
ctype (BasicTypeFloat 32) = ([CTypeSpec $ u CFloatType], [])
ctype (BasicTypeFloat 64) = ([CTypeSpec $ u CDoubleType], [])
ctype (BasicTypeFloat _ ) = undefined
ctype (BasicTypeStruct name) =
  ( [ CTypeSpec $ u $ CSUType $ u $ CStruct CStructTag
                                            (Just $ cIdent $ mangleName name)
                                            Nothing
                                            []
    ]
  , []
  )
ctype (BasicTypeAnonStruct Nothing args) =
  ( [ CTypeSpec $ u $ CSUType $ u $ CStruct
        CStructTag
        Nothing
        (Just
          [ cDecl argType (Just ([], argName)) Nothing
          | (argName, argType) <- args
          ]
        )
        []
    ]
  , []
  )
ctype (BasicTypeUnion name) =
  ( [ CTypeSpec $ u $ CSUType $ u $ CStruct CUnionTag
                                            (Just $ cIdent $ mangleName name)
                                            Nothing
                                            []
    ]
  , []
  )
ctype (BasicTypeAnonUnion Nothing args) =
  ( [ CTypeSpec $ u $ CSUType $ u $ CStruct
        CUnionTag
        Nothing
        (Just
          [ cDecl argType (Just ([], argName)) Nothing
          | (argName, argType) <- args
          ]
        )
        []
    ]
  , []
  )
ctype (BasicTypeSimpleEnum name) =
  ( [ CTypeSpec $ u $ CEnumType $ u $ CEnum (Just (cIdent $ mangleName name))
                                            Nothing
                                            []
    ]
  , []
  )
-- ctype (BasicTypeAnonEnum variants) =
--   ( [ u $ CEnumType $ u $ CEnum Nothing
--                                 (Just )
--                                 []
--     ]
--   , []
--   )
ctype (BasicTypeComplexEnum name) = ctype (BasicTypeStruct name)
ctype (BasicTypeTuple name t) =
  ( [ CTypeSpec $ u $ CSUType $ u $ CStruct CStructTag
                                            (Just (cIdent name))
                                            Nothing
                                            []
    ]
  , []
  )
ctype (CPtr x) = (fst t, (u $ CPtrDeclr []) : snd t) where t = ctype x
ctype (BasicTypeFunction rt args var) =
  let (rta, rtb) = ctype rt
  in  ( rta
      , (u $ CPtrDeclr [])
      : (u $ CFunDeclr
          (Right ([ cDecl t Nothing Nothing | (name, t) <- args ], var))
          []
        )
      : rtb
      )
ctype (BasicTypeCFile) =
  ([CTypeSpec $ u $ CTypeDef (internalIdent $ "FILE")], [])
ctype (BasicTypeAnonEnum Nothing variants)
  = ( [ CTypeSpec $ u $ CEnumType $ u $ CEnum
          Nothing
          (Just
            [ (internalIdent $ s_unpack variant, Nothing)
            | variant <- variants
            ]
          )
          []
      ]
    , []
    )
ctype (CArray x s) =
  ( fst t
  , (u $ CArrDeclr
      []
      (case s of
        Just i ->
          CArrSize False (transpileExpr $ IrLiteral (IntValue i) BasicTypeCInt)
        Nothing -> CNoArrSize False
      )
    )
    : snd t
  )
  where t = ctype x
ctype (BasicTypeAnonStruct (Just x) _) =
  ([CTypeSpec $ u $ CTypeDef $ cIdent x], [])
ctype (BasicTypeAnonUnion (Just x) _) =
  ([CTypeSpec $ u $ CTypeDef $ cIdent x], [])
ctype (BasicTypeAnonEnum (Just x) _) =
  ([CTypeSpec $ u $ CTypeDef $ cIdent x], [])
ctype (BasicTypeTypedef x) = ([CTypeSpec $ u $ CTypeDef $ cIdent x], [])
ctype (BasicTypeUnknown  ) = undefined
-- ctype (t) = throwk $ InternalError ("Unhandled ctype: " ++ show t) Nothing

intFlags f = foldr (\f acc -> setFlag f acc) noFlags f

initializerExpr :: IrExpr -> CInit
initializerExpr (IrCArrLiteral values _) =
  u $ CInitList $ [ ([], x) | x <- map initializerExpr values ]
initializerExpr (IrStructInit _ fields) =
  u
    $ CInitList
    $ [ ([u $ CMemberDesig (cIdent name)], initializerExpr value)
      | (name, value) <- fields
      ]
initializerExpr (IrUnionInit _ (name, value)) =
  u $ CInitList $ [([u $ CMemberDesig (cIdent name)], initializerExpr value)]
initializerExpr x = u $ CInitExpr $ transpileExpr x

transpileExpr :: IrExpr -> CExpr
transpileExpr (IrIdentifier s) = u $ CVar $ cIdent $ mangleName s
transpileExpr (IrLiteral (BoolValue b) _) =
  CConst $ u $ CIntConst $ cInteger (if b then 1 else 0)
transpileExpr (IrLiteral (IntValue i) t@(BasicTypeFloat _)) =
  transpileExpr (IrLiteral (FloatValue (s_pack $ show i)) t)
transpileExpr (IrLiteral (IntValue i) t) = CConst $ u $ CIntConst $ CInteger
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
transpileExpr (IrLiteral (FloatValue f) t) =
  CConst $ u $ CFloatConst $ transpileFloat (s_unpack f)
transpileExpr (IrLiteral (StringValue s) _) =
  u
    $ CCast (cDecl (CPtr $ BasicTypeCChar) Nothing Nothing)
    $ CConst
    $ u
    $ CStrConst
    $ cString
    $ s_unpack s
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
transpileExpr (IrField e s) = u $ CMember (transpileExpr e) (cIdent s) False
transpileExpr (IrArrayAccess e1 e2) =
  u $ CIndex (transpileExpr e1) (transpileExpr e2)
transpileExpr (IrCall e args) =
  u $ CCall (transpileExpr e) [ transpileExpr x | x <- args ]
transpileExpr (IrCast e t) =
  u $ CCast (cDecl t Nothing Nothing) (transpileExpr e)
transpileExpr (IrCArrLiteral values x) = u $ CCompoundLit
  arrDecl
  [ ([], u $ CInitExpr (transpileExpr val)) | val <- values ]
 where
  t = ctype x
  arrT =
    ( fst t
    , (u $ CArrDeclr
        []
        (CArrSize
          False
          (transpileExpr $ IrLiteral (IntValue $ length values) BasicTypeCInt)
        )
      )
      : snd t
    )
  arrDecl = u $ CDecl typeSpec [(Just cdeclr, Nothing, Nothing)]
   where
    (typeSpec, derivedDeclr) = arrT
    cdeclr                   = u $ CDeclr Nothing derivedDeclr Nothing []
transpileExpr (IrStructInit t fields) = u $ CCompoundLit
  (cDecl t Nothing Nothing)
  [ ([u $ CMemberDesig (cIdent name)], u $ CInitExpr (transpileExpr e))
  | (name, e) <- fields
  ]
transpileExpr (IrUnionInit t (name, e)) = u $ CCompoundLit
  (cDecl t Nothing Nothing)
  [([u $ CMemberDesig (cIdent name)], u $ CInitExpr (transpileExpr e))]
transpileExpr (IrEnumInit (BasicTypeSimpleEnum _) discriminant []) =
  transpileExpr (IrIdentifier discriminant)
transpileExpr (IrEnumInit (BasicTypeAnonEnum _ _) discriminant []) =
  transpileExpr (IrIdentifier discriminant)
transpileExpr (IrEnumInit t discriminant []) = u $ CCompoundLit
  (cDecl t Nothing Nothing)
  [ ( [u $ CMemberDesig (cIdent discriminantFieldName)]
    , u $ CInitExpr $ transpileExpr (IrIdentifier discriminant)
    )
  ]
transpileExpr (IrEnumInit t@(BasicTypeComplexEnum name) discriminant fields) =
  u $ CCompoundLit
    (cDecl t Nothing Nothing)
    [ ( [u $ CMemberDesig (cIdent discriminantFieldName)]
      , u $ CInitExpr $ transpileExpr (IrIdentifier discriminant)
      )
    , ( [ u
          $  CMemberDesig
          $  internalIdent
          $  s_unpack (s_concat [variantFieldName, ".variant_"])
          ++ (s_unpack $ tpName discriminant)
        ]
      , u $ CInitExpr $ transpileExpr
        (IrStructInit (BasicTypeStruct $ subPath discriminant "data") fields)
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
transpileExpr (IrIf c e1 (Just e2)) =
  u $ CCond (transpileExpr c) (Just $ transpileExpr e1) (transpileExpr e2)
transpileExpr (IrNull              ) = transpileExpr $ IrIdentifier ([], "NULL")
transpileExpr (IrEmpty (CArray _ _)) = transpileExpr $ IrIdentifier ([], "{0}")
transpileExpr (IrEmpty t           ) = u $ CCompoundLit
  (cDecl t Nothing Nothing)
  [([], u $ CInitExpr $ transpileExpr $ IrIdentifier ([], "0"))]
transpileExpr (IrInlineC s) = transpileExpr $ IrIdentifier ([], s) -- what a hack
transpileExpr (IrType t) =
  u $ CVar $ internalIdent $ show $ pretty $ cDecl t Nothing Nothing
transpileExpr x =
  throwk $ InternalError ("Couldn't transpile IR:\n\n  " ++ show x) Nothing

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
  ( Right
  $ cDecl idType (Just ([], v)) (Just $ u $ CInitExpr $ transpileExpr start)
  )
  (Just $ transpileExpr (IrBinop Lt (IrIdentifier ([], v)) (end)))
  (Just $ transpileExpr (IrPreUnop Inc (IrIdentifier ([], v))))
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

var_to_cdeclr x = u $ CDeclr (Just $ cIdent x) [] (Nothing) []

transpileBlockItem :: IrExpr -> CBlockItem
transpileBlockItem (IrVarDeclaration v t (Just (IrCArrLiteral values _))) =
  CBlockDecl $ cDecl t (Just ([], v)) $ Just $ u $ CInitList
    [ ([], initializerExpr val) | val <- values ]
transpileBlockItem (IrVarDeclaration v t varDefault) = CBlockDecl
  $ cDecl t (Just ([], v)) body
 where
  body = case varDefault of
    Just x  -> Just $ initializerExpr x
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
  readBin _   = undefined
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
