module Kit.Ast.Definitions.FunctionDefinition where

import Control.Monad
import Kit.Ast.Definitions.Base
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Parser.Span
import Kit.Str

data FunctionDefinition a b = FunctionDefinition {
  functionName :: Str,
  functionDoc :: Maybe Str,
  functionMeta :: [Metadata],
  functionModifiers :: [Modifier],
  functionParams :: [TypeParam],
  functionArgs :: [ArgSpec a b],
  functionType :: b,
  functionBody :: Maybe a,
  functionVarargs :: Bool,
  functionNamespace :: [Str],
  functionThis :: Maybe b,
  functionSelf :: Maybe b
} deriving (Eq, Show)

newFunctionDefinition :: FunctionDefinition a b
newFunctionDefinition = FunctionDefinition
  { functionName      = undefined
  , functionDoc       = Nothing
  , functionMeta      = []
  , functionModifiers = [Public]
  , functionParams    = []
  , functionArgs      = []
  , functionType      = undefined
  , functionBody      = Nothing
  , functionVarargs   = False
  , functionNamespace = []
  , functionThis      = Nothing
  , functionSelf      = Nothing
  }

convertFunctionDefinition
  :: (Monad m)
  => (a -> m c)
  -> (b -> m d)
  -> [ArgSpec c d]
  -> d
  -> FunctionDefinition a b
  -> m (FunctionDefinition c d)
convertFunctionDefinition exprConverter typeConverter newArgs newType f = do
  newBody <- maybeConvert exprConverter (functionBody f)
  return $ (newFunctionDefinition) { functionName      = functionName f
                                   , functionDoc       = functionDoc f
                                   , functionMeta      = functionMeta f
                                   , functionModifiers = functionModifiers f
                                   , functionParams    = functionParams f
                                   , functionArgs      = newArgs
                                   , functionType      = newType
                                   , functionBody      = newBody
                                   , functionVarargs   = functionVarargs f
                                   , functionNamespace = functionNamespace f
                                   }

data ArgSpec a b = ArgSpec {
  argName :: Str,
  argType :: b,
  argDefault :: Maybe a,
  argPos :: Span
} deriving (Show)

instance (Eq a, Eq b) => Eq (ArgSpec a b) where
  (==) a b = (argName a == argName b) && (argType a == argType b) && (argDefault a == argDefault b)

newArgSpec = ArgSpec
  { argName    = undefined
  , argType    = Nothing
  , argDefault = Nothing
  , argPos     = null_span
  }

convertArgSpec
  :: (Monad m) => (a -> m c) -> (b -> m d) -> ArgSpec a b -> m (ArgSpec c d)
convertArgSpec exprConverter typeConverter a = do
  newType    <- typeConverter (argType a)
  newDefault <- maybeConvert exprConverter (argDefault a)
  return $ newArgSpec { argName    = argName a
                      , argType    = newType
                      , argDefault = newDefault
                      , argPos     = argPos a
                      }
