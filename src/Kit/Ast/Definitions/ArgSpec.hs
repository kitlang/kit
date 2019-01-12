{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.Definitions.ArgSpec(
  ArgSpec (..),
  newArgSpec,
  convertArgSpec
) where

import Control.Monad
import Data.Hashable
import GHC.Generics
import Kit.Ast.Definitions.Base
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.TypePath
import Kit.Ast.Span
import Kit.Str

data ArgSpec a b = ArgSpec {
  argName :: Str,
  argType :: b,
  argDefault :: Maybe a,
  argPos :: Span
} deriving (Generic, Show)

instance Positioned (ArgSpec a b) where
  position = argPos

instance (Hashable a, Hashable b) => Hashable (ArgSpec a b) where
  hashWithSalt = hashUsing argType

instance (Eq a, Eq b) => Eq (ArgSpec a b) where
  (==) a b = (argType a == argType b)

newArgSpec = ArgSpec
  { argName    = undefined
  , argType    = Nothing
  , argDefault = Nothing
  , argPos     = NoPos
  }

convertArgSpec
  :: (Monad m) => Converter m a b c d -> ArgSpec a b -> m (ArgSpec c d)
convertArgSpec (Converter { exprConverter = exprConverter, typeConverter = typeConverter }) a
  = do
    newType    <- typeConverter (argPos a) (argType a)
    newDefault <- maybeConvert exprConverter (argDefault a)
    return $ newArgSpec { argName    = argName a
                        , argType    = newType
                        , argDefault = newDefault
                        , argPos     = argPos a
                        }
