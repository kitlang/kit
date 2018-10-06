{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.Definitions.FunctionDefinition where

import Control.Monad
import GHC.Generics
import Kit.Ast.Definitions.Base
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.TypeParam
import Kit.Ast.TypePath
import Kit.Ast.Types
import Kit.NameMangling
import Kit.Ast.Span
import Kit.Str

data FunctionDefinition a b = FunctionDefinition {
  functionName :: TypePath,
  functionMonomorph :: [b],
  functionBundle :: Maybe TypePath,
  functionPos :: Span,
  functionDoc :: Maybe Str,
  functionMeta :: [Metadata],
  functionModifiers :: [Modifier],
  functionParams :: [TypeParam b],
  functionArgs :: [ArgSpec a b],
  functionType :: b,
  functionBody :: Maybe a,
  functionVarargs :: Bool
} deriving (Eq, Generic, Show)

functionSubPath :: FunctionDefinition a b -> Str -> TypePath
functionSubPath def s = if hasMeta "extern" (functionMeta def)
  then ([], s)
  else subPath (functionName def) s

functionRealName f = if hasMeta "extern" (functionMeta f)
  then ([], tpName $ functionName f)
  else monomorphName (functionName f) (functionMonomorph f)


newFunctionDefinition :: FunctionDefinition a b
newFunctionDefinition = FunctionDefinition
  { functionName      = undefined
  , functionMonomorph = []
  , functionBundle    = Nothing
  , functionDoc       = Nothing
  , functionMeta      = []
  , functionModifiers = [Public]
  , functionParams    = []
  , functionArgs      = []
  , functionType      = undefined
  , functionBody      = Nothing
  , functionVarargs   = False
  , functionPos       = NoPos
  }

convertFunctionDefinition
  :: (Monad m)
  => ParameterizedConverter m a b c d
  -> FunctionDefinition a b
  -> m (FunctionDefinition c d)
convertFunctionDefinition paramConverter f = do
  let params =
        [ functionSubPath f $ paramName param | param <- functionParams f ]
  let
    converter@(Converter { exprConverter = exprConverter, typeConverter = typeConverter })
      = paramConverter params
  rt     <- typeConverter (functionPos f) (functionType f)
  args   <- forM (functionArgs f) (convertArgSpec converter)
  body   <- maybeConvert exprConverter (functionBody f)
  params <- forM (functionParams f) $ convertTypeParam converter

  return $ (newFunctionDefinition) { functionName      = functionName f
                                   , functionBundle    = functionBundle f
                                   , functionDoc       = functionDoc f
                                   , functionMeta      = functionMeta f
                                   , functionModifiers = functionModifiers f
                                   , functionParams    = params
                                   , functionArgs      = args
                                   , functionType      = rt
                                   , functionBody      = body
                                   , functionVarargs   = functionVarargs f
                                   , functionPos       = functionPos f
                                   }

data ArgSpec a b = ArgSpec {
  argName :: Str,
  argType :: b,
  argDefault :: Maybe a,
  argPos :: Span
} deriving (Generic, Show)

instance (Eq a, Eq b) => Eq (ArgSpec a b) where
  (==) a b = (argName a == argName b) && (argType a == argType b) && (argDefault a == argDefault b)

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

implicitifyMethod
  :: Str
  -> b
  -> (FunctionDefinition a b -> a -> a)
  -> FunctionDefinition a b
  -> FunctionDefinition a b
implicitifyMethod thisName thisType body method = method
  { functionArgs = (newArgSpec { argName = thisName
                               , argType = thisType
                               , argPos  = functionPos method
                               }
                   )
    : (functionArgs method)
  , functionBody = case functionBody method of
    Just x  -> Just $ body method x
    Nothing -> Nothing
  }

reimplicitify :: b -> FunctionDefinition a b -> FunctionDefinition a b
reimplicitify selfType def = def
  { functionArgs = ((head $ functionArgs def) { argType = selfType })
    : (tail $ functionArgs def)
  }

-- functionConcrete :: FunctionDefinition TypedExpr ConcreteType -> ConcreteType
-- functionConcrete f = TypeFunction
--   (functionType f)
--   [ (argName arg, argType arg) | arg <- functionArgs f ]
--   Bool
--   []
