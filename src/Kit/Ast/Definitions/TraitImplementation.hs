module Kit.Ast.Definitions.TraitImplementation where

import Control.Monad
import Kit.Ast.Definitions.RewriteRule
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Str

data TraitImplementation a b = TraitImplementation {
  implTrait :: b,
  implFor :: b,
  implRules :: [RewriteRuleType a b],
  implDoc :: Maybe Str
} deriving (Eq, Show)
