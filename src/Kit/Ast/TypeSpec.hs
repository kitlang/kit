module Kit.Ast.TypeSpec where

  import Data.Hashable
  import Data.List
  import Kit.Ast.BasicType
  import Kit.Ast.ConcreteType
  import Kit.Ast.ModulePath
  import Kit.Str

  type TypeSpecArgs = [(Str, TypeSpec)]

  {-
    A TypeSpec is a syntactic type. TypeSpecs try to resolve to a specific
    ConcreteType when expressions are typed.
  -}
  data TypeSpec
    = TypeSpec TypePath [TypeParam]
    | TypeFunctionSpec TypeSpec TypeSpecArgs Bool
    {-
      This variant can be used to force the BasicType to resolve to a specific
      ConcreteType without going through normal namespace resolution. This is
      used when we already know the underlying type when generating the AST,
      e.g. for C externs.
    -}
    | ConcreteType ConcreteType
    deriving (Eq)

  instance Show TypeSpec where
    show (TypeSpec (tp) params) = (s_unpack $ showTypePath tp) ++ (if params == [] then "" else "[" ++ (intercalate "," [show param | param <- params]) ++ "]")

  data TypeParam = TypeParam {
    param_type :: TypeSpec,
    constraints :: [TypeSpec]
  } deriving (Eq, Show)

  typeSpecToBasicType :: TypeSpec -> Maybe BasicType
  typeSpecToBasicType (TypeSpec ([], "CString") []) = Just $ CPtr $ BasicTypeInt 8
  typeSpecToBasicType (TypeSpec ([], "Bool") []) = Just $ BasicTypeBool
  typeSpecToBasicType (TypeSpec ([], "Int") []) = typeSpecToBasicType (TypeSpec ([], "Int32") [])
  typeSpecToBasicType (TypeSpec ([], "Int8") []) = Just $ BasicTypeInt 8
  typeSpecToBasicType (TypeSpec ([], "Int16") []) = Just $ BasicTypeInt 16
  typeSpecToBasicType (TypeSpec ([], "Int32") []) = Just $ BasicTypeInt 32
  typeSpecToBasicType (TypeSpec ([], "Int64") []) = Just $ BasicTypeInt 64
  typeSpecToBasicType (TypeSpec ([], "Uint8") []) = Just $ BasicTypeUint 8
  typeSpecToBasicType (TypeSpec ([], "Uint16") []) = Just $ BasicTypeUint 16
  typeSpecToBasicType (TypeSpec ([], "Uint32") []) = Just $ BasicTypeUint 32
  typeSpecToBasicType (TypeSpec ([], "Uint64") []) = Just $ BasicTypeUint 64
  typeSpecToBasicType (TypeSpec ([], "Float32") []) = Just $ BasicTypeFloat 32
  typeSpecToBasicType (TypeSpec ([], "Float64") []) = Just $ BasicTypeFloat 64
  typeSpecToBasicType _ = Nothing
