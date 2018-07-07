module Kit.Ast.TypeSpec where

  import Data.Hashable
  import Kit.Ast.Base
  import Kit.Ast.BasicType
  import Kit.Ast.ConcreteType
  import Kit.Str

  {-
    A TypeSpec is a syntactic type.
    TypeSpec resolves to a ConcreteType (or fail to resolve)
    which will become an underlying BasicType at runtime
  -}
  data TypeSpec
    = TypeSpec TypePath [TypeParam]
    | TypeVar TypeVar
    | TypeFunctionSpec TypeSpec [(Str, TypeSpec)] Bool
    | ConcreteType ConcreteType
    deriving (Eq, Show)

  data TypeParam = TypeParam {
    param_type :: TypeSpec,
    constraints :: [TypeConstraint]
  } deriving (Eq, Show)

  typeSpecToBasicType :: TypeSpec -> Maybe BasicType
  typeSpecToBasicType (TypeSpec ([], "CString") []) = Just $ CString
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

  data TypeConstraint
    = TypeEq TypeSpec TypeSpec
    | TypeNumeric TypeSpec
    | TypeFloating TypeSpec
    | TypeString TypeSpec
    | TypeArray TypeSpec
    deriving (Eq, Show)
  type TypeVar = Int
