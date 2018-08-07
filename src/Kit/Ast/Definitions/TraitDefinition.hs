module Kit.Ast.Definitions.TraitDefinition where

import Control.Monad
import Kit.Ast.Definitions.Base
import Kit.Ast.Definitions.FunctionDefinition
import Kit.Ast.Definitions.RewriteRule
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Parser.Span
import Kit.Str

data TraitDefinition a b = TraitDefinition {
  traitName :: Str,
  traitPos :: Span,
  traitDoc :: Maybe Str,
  traitMeta :: [Metadata],
  traitModifiers :: [Modifier],
  traitParams :: [TypeParam],
  traitRules :: [RewriteRule a b],
  traitMethods :: [FunctionDefinition a b]
} deriving (Eq, Show)

newTraitDefinition = TraitDefinition
  { traitName      = undefined
  , traitPos       = NoPos
  , traitDoc       = Nothing
  , traitMeta      = []
  , traitModifiers = []
  , traitParams    = []
  , traitRules     = []
  , traitMethods   = []
  }

convertTraitDefinition
  :: (Monad m)
  => ParameterizedConverter m a b c d
  -> TraitDefinition a b
  -> m (TraitDefinition c d)
convertTraitDefinition paramConverter t = do
  let params = map paramName (traitParams t)
  let
    converter@(Converter { exprConverter = exprConverter, typeConverter = typeConverter })
      = paramConverter params
  methods <- forM
    (traitMethods t)
    (convertFunctionDefinition (\p -> paramConverter (p ++ (map paramName $ traitParams t))))
  return $ (newTraitDefinition) { traitName      = traitName t
                                , traitPos       = traitPos t
                                , traitDoc       = traitDoc t
                                , traitMeta      = traitMeta t
                                , traitModifiers = traitModifiers t
                                , traitParams    = traitParams t
                                , traitMethods   = methods
                                }
