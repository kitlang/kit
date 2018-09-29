module Kit.Ast.Definitions.TraitDefinition where

import Control.Monad
import Kit.Ast.Definitions.Base
import Kit.Ast.Definitions.FunctionDefinition
import Kit.Ast.Definitions.RewriteRule
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.TypePath
import Kit.Ast.TypeSpec
import Kit.NameMangling
import Kit.Ast.Span
import Kit.Str

data TraitDefinition a b = TraitDefinition {
  traitName :: TypePath,
  traitMonomorph :: [b],
  traitPos :: Span,
  traitDoc :: Maybe Str,
  traitMeta :: [Metadata],
  traitModifiers :: [Modifier],
  traitParams :: [TypeParam],
  traitAssocParams :: [TypeParam],
  traitRules :: [RewriteRule a b],
  traitMethods :: [FunctionDefinition a b]
} deriving (Eq, Show)

traitSubPath :: TraitDefinition a b -> Str -> TypePath
traitSubPath def s = subPath (traitName def) s

traitAllParams t = traitParams t ++ traitAssocParams t
traitExplicitParams def = take (length $ traitParams def)

traitRealName t = monomorphName (traitName t) (traitMonomorph t)

newTraitDefinition = TraitDefinition
  { traitName        = undefined
  , traitMonomorph   = []
  , traitPos         = NoPos
  , traitDoc         = Nothing
  , traitMeta        = []
  , traitModifiers   = []
  , traitParams      = []
  , traitAssocParams = []
  , traitRules       = []
  , traitMethods     = []
  }

convertTraitDefinition
  :: (Monad m)
  => ParameterizedConverter m a b c d
  -> TraitDefinition a b
  -> m (TraitDefinition c d)
convertTraitDefinition paramConverter t = do
  let params = [ traitSubPath t $ paramName param | param <- traitParams t ]
  let
    converter@(Converter { exprConverter = exprConverter, typeConverter = typeConverter })
      = paramConverter params
  let methodParamConverter methodParams =
        paramConverter (methodParams ++ params)
  methods <- forM
    (traitMethods t)
    (\f -> convertFunctionDefinition methodParamConverter
      $ f { functionName = traitSubPath t (tpName $ functionName f) }
    )
  rules <- forM (traitRules t) $ convertRewriteRule converter
  return $ (newTraitDefinition) { traitName        = traitName t
                                , traitPos         = traitPos t
                                , traitDoc         = traitDoc t
                                , traitMeta        = traitMeta t
                                , traitModifiers   = traitModifiers t
                                , traitParams      = traitParams t
                                , traitAssocParams = traitAssocParams t
                                , traitMethods     = methods
                                , traitRules       = rules
                                }

valuePointerName :: Str
valuePointerName = "__this"
vtablePointerName :: Str
vtablePointerName = "__vtable"
