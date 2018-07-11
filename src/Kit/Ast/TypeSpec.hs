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
    | TypeFunctionSpec TypeSpec [TypeParam] TypeSpecArgs Bool
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
