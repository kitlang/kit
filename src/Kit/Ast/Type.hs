module Kit.Ast.Type where

  import Kit.Ast.BasicType
  import Kit.Str

  type TypePath = ([Str], Str)
  type ParameterizedType = (TypePath, [TypeParam])

  data TypeSpec
    = ParameterizedTypePath ParameterizedType
    | FunctionType [TypeSpec] TypeSpec
    deriving (Eq, Show)

  data TypeParam = TypeParam {
    t :: ParameterizedType,
    constraints :: [TypeSpec]
  } deriving (Eq, Show)

  data ConcreteType
    = BasicType BasicType
    | UserType ParameterizedType
    | ConcreteFunctionType [ConcreteType] TypeSpec
    | TypeVar TypeVarId
    deriving (Eq, Show)

  type TypeVarId = Int
