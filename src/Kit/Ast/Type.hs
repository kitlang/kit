module Kit.Ast.Type where

  import qualified Data.ByteString.Lazy.Char8 as B

  type TypePath = ([B.ByteString], B.ByteString)
  type ParameterizedType = (TypePath, [TypeParam])

  data TypeSpec
    = ParameterizedTypePath ParameterizedType
    | FunctionType [TypeSpec] TypeSpec
    deriving (Eq, Show)

  data TypeParam = TypeParam {
    t :: ParameterizedType,
    constraints :: [TypeSpec]
  } deriving (Eq, Show)
