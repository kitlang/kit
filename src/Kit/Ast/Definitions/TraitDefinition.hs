{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.Definitions.TraitDefinition where

import Control.Monad
import Data.Hashable
import GHC.Generics
import Kit.Ast.Definitions.Base
import Kit.Ast.Definitions.FunctionDefinition
import Kit.Ast.Definitions.RewriteRule
import Kit.Ast.Definitions.VarDefinition
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.TypeParam
import Kit.Ast.TypePath
import Kit.Ast.Types
import Kit.NameMangling
import Kit.Ast.Span
import Kit.Str

data TraitDefinition a b = TraitDefinition {
  traitName :: TypePath,
  traitMonomorph :: [b],
  traitPos :: Span,
  traitMeta :: [Metadata],
  traitModifiers :: [Modifier],
  traitParams :: [TypeParam b],
  traitAssocParams :: [TypeParam b],
  traitRules :: [RewriteRule a b],
  traitMethods :: [FunctionDefinition a b],
  traitStaticFields :: [VarDefinition a b],
  traitStaticMethods :: [FunctionDefinition a b]
} deriving (Eq, Generic, Show)

instance (Hashable a, Hashable b) => Hashable (TraitDefinition a b)

instance Positioned (TraitDefinition a b) where
  position = traitPos

traitSubPath :: TraitDefinition a b -> Str -> TypePath
traitSubPath def s = subPath (traitName def) s

traitAllParams t = traitParams t ++ traitAssocParams t
traitExplicitParams def = take (length $ traitParams def)

traitRealName t = monomorphName (traitName t) (traitMonomorph t)

newTraitDefinition = TraitDefinition
  { traitName          = undefined
  , traitMonomorph     = []
  , traitPos           = NoPos
  , traitMeta          = []
  , traitModifiers     = []
  , traitParams        = []
  , traitAssocParams   = []
  , traitRules         = []
  , traitMethods       = []
  , traitStaticFields  = []
  , traitStaticMethods = []
  }

convertTraitDefinition
  :: (Monad m)
  => ParameterizedConverter m a b c d
  -> TraitDefinition a b
  -> m (TraitDefinition c d)
convertTraitDefinition paramConverter t = do
  let params = [ traitSubPath t $ paramName param | param <- traitParams t ]
  converter@(Converter { exprConverter = exprConverter, typeConverter = typeConverter })
      <- paramConverter params
  let methodParamConverter methodParams =
        paramConverter (methodParams ++ params)
  methods <- forM
    (traitMethods t)
    (\f -> convertFunctionDefinition methodParamConverter
      $ f { functionName = traitSubPath t (tpName $ functionName f) }
    )
  staticFields <- forM
    (traitStaticFields t)
    (\v -> convertVarDefinition converter
      $ v { varName = traitSubPath t (tpName $ varName v) }
    )
  staticMethods <- forM
    (traitStaticMethods t)
    (\f -> convertFunctionDefinition methodParamConverter
      $ f { functionName = traitSubPath t (tpName $ functionName f) }
    )
  rules       <- forM (traitRules t) $ convertRewriteRule converter
  params      <- forM (traitParams t) $ convertTypeParam converter
  assocParams <- forM (traitAssocParams t) $ convertTypeParam converter
  return $ (newTraitDefinition) { traitName          = traitName t
                                , traitPos           = traitPos t
                                , traitMeta          = traitMeta t
                                , traitModifiers     = traitModifiers t
                                , traitParams        = params
                                , traitAssocParams   = assocParams
                                , traitMethods       = methods
                                , traitStaticFields  = staticFields
                                , traitStaticMethods = staticMethods
                                , traitRules         = rules
                                }

valuePointerName :: Str
valuePointerName = "__this"
vtablePointerName :: Str
vtablePointerName = "__vtable"
