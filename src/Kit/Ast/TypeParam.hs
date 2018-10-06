{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.TypeParam where

import Control.Monad
import Data.List
import GHC.Generics
import Kit.Ast.Definitions.Base
import Kit.Ast.TypePath
import Kit.Ast.Types
import Kit.Ast.Value
import Kit.Ast.Span
import Kit.Str

data TypeParam b = TypeParam {
  paramName :: Str,
  typeParamPos :: Span,
  constraints :: [b],
  typeParamIsConstant :: Bool,
  typeParamDefault :: Maybe b
} deriving (Eq, Generic, Show)

makeTypeParam :: Str -> TypeParam b
makeTypeParam s = TypeParam
  { paramName           = s
  , typeParamPos        = NoPos
  , constraints         = []
  , typeParamIsConstant = False
  , typeParamDefault    = Nothing
  }
typeParamToSpec (TypeParam { paramName = s }) = makeTypeSpec s

convertTypeParam
  :: (Monad m) => Converter m a b c d -> TypeParam b -> m (TypeParam d)
convertTypeParam converter param = do
  constraints <- forM (constraints param)
    $ typeConverter converter (typeParamPos param)
  def <- maybeConvert (typeConverter converter $ typeParamPos param)
                      (typeParamDefault param)
  return $ (makeTypeParam $ paramName param)
    { typeParamPos        = typeParamPos param
    , constraints         = constraints
    , typeParamIsConstant = typeParamIsConstant param
    , typeParamDefault    = def
    }
