module Kit.Ast.Definitions.TraitDefinition where

import Control.Monad
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
