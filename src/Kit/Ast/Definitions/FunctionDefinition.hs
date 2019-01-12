{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.Definitions.FunctionDefinition (
  FunctionDefinition (..),
  functionSubPath,
  newFunctionDefinition,
  convertFunctionDefinition,
  implicitifyMethod,
  functionRealName
) where

import Control.Monad
import Data.Hashable
import GHC.Generics
import Kit.Ast.Definitions.Base
import Kit.Ast.Definitions.ArgSpec
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.TypeParam
import Kit.Ast.TypePath
import Kit.NameMangling
import Kit.Ast.Span
import Kit.Str

data FunctionDefinition a b = FunctionDefinition {
  functionName :: TypePath,
  functionMonomorph :: [b],
  functionBundle :: Maybe TypePath,
  functionPos :: Span,
  functionMeta :: [Metadata],
  functionModifiers :: [Modifier],
  functionParams :: [TypeParam b],
  functionArgs :: [ArgSpec a b],
  functionType :: b,
  functionBody :: Maybe a,
  functionVararg :: Maybe Str
} deriving (Eq, Generic, Show)

instance Positioned (FunctionDefinition a b) where
  position = functionPos

instance (Hashable a, Hashable b) => Hashable (FunctionDefinition a b)

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
  , functionMeta      = []
  , functionModifiers = [Public]
  , functionParams    = []
  , functionArgs      = []
  , functionType      = undefined
  , functionBody      = Nothing
  , functionVararg    = Nothing
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
  converter@(Converter { exprConverter = exprConverter, typeConverter = typeConverter }) <-
    paramConverter params
  rt     <- typeConverter (functionPos f) (functionType f)
  args   <- forM (functionArgs f) (convertArgSpec converter)
  body   <- maybeConvert exprConverter (functionBody f)
  params <- forM (functionParams f) $ convertTypeParam converter

  return $ (newFunctionDefinition) { functionName      = functionName f
                                   , functionBundle    = functionBundle f
                                   , functionMeta      = functionMeta f
                                   , functionModifiers = functionModifiers f
                                   , functionParams    = params
                                   , functionArgs      = args
                                   , functionType      = rt
                                   , functionBody      = body
                                   , functionVararg    = functionVararg f
                                   , functionPos       = functionPos f
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
