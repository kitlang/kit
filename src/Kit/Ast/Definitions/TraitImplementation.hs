{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.Definitions.TraitImplementation where

import Control.Monad
import Data.Hashable
import GHC.Generics
import Kit.Ast.Definitions.Base
import Kit.Ast.Definitions.FunctionDefinition
import Kit.Ast.Definitions.TraitDefinition
import Kit.Ast.Definitions.VarDefinition
import Kit.Ast.TypeParam
import Kit.Ast.TypePath
import Kit.Ast.Types
import Kit.Ast.Span
import Kit.Str

data TraitImplementation a b = TraitImplementation {
  implName :: TypePath,
  implTrait :: b,
  implFor :: b,
  implParams :: [TypeParam b],
  implAssocTypes :: [b],
  implMethods :: [FunctionDefinition a b],
  implStaticFields :: [VarDefinition a b],
  implStaticMethods :: [FunctionDefinition a b],
  implPos :: Span
} deriving (Eq, Generic, Show)

instance (Hashable a, Hashable b) => Hashable (TraitImplementation a b)

instance Positioned (TraitImplementation a b) where
  position = implPos

associatedTypes traitDef impl =
  [ (traitSubPath traitDef $ paramName p, t)
  | (p, t) <- zip (traitAssocParams traitDef) (implAssocTypes impl)
  ]

newTraitImplementation = TraitImplementation
  { implName          = undefined
  , implTrait         = undefined
  , implFor           = undefined
  , implParams        = []
  , implAssocTypes    = []
  , implMethods       = []
  , implStaticFields  = []
  , implStaticMethods = []
  , implPos           = NoPos
  }

convertTraitImplementation
  :: (Monad m)
  => Converter m a b c d
  -> TraitImplementation a b
  -> m (TraitImplementation c d)
convertTraitImplementation converter@(Converter { exprConverter = exprConverter, typeConverter = typeConverter }) i
  = do
    trait      <- typeConverter (implPos i) (implTrait i)
    for        <- typeConverter (implPos i) (implFor i)
    assocTypes <- forM (implAssocTypes i) $ typeConverter (implPos i)
    methods    <- forM (implMethods i)
                       (\f -> convertFunctionDefinition (\p -> converter) f)
    staticFields <- forM (implStaticFields i)
                         (\f -> convertVarDefinition converter f)
    staticMethods <- forM
      (implStaticMethods i)
      (\f -> convertFunctionDefinition (\p -> converter) f)
    params <- forM (implParams i) $ convertTypeParam converter
    return $ (newTraitImplementation) { implName          = implName i
                                      , implTrait         = trait
                                      , implFor           = for
                                      , implParams        = params
                                      , implAssocTypes    = assocTypes
                                      , implMethods       = methods
                                      , implStaticFields  = staticFields
                                      , implStaticMethods = staticMethods
                                      , implPos           = implPos i
                                      }

vThisArgName :: Str
vThisArgName = "__vthis"
