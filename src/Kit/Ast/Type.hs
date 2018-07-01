{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.Type where

  import GHC.Generics (Generic)
  import Data.Hashable
  import Kit.Ir.BasicType
  import Kit.Str

  type TypePath = ([Str], Str)
  type ParameterizedType = (TypePath, [TypeParam])

  data TypeSpec
    = ParameterizedTypePath ParameterizedType
    | FunctionType [TypeSpec] TypeSpec
    deriving (Eq, Show, Generic)

  instance Hashable TypeSpec

  data TypeParam = TypeParam {
    t :: ParameterizedType,
    constraints :: [TypeSpec]
  } deriving (Eq, Show, Generic)

  instance Hashable TypeParam

  data ConcreteType
    = BasicType BasicType
    | UserType ParameterizedType
    | ConcreteFunctionType [ConcreteType] TypeSpec
    | TypeVar TypeVarId
    deriving (Eq, Show)

  type TypeVarId = Int
