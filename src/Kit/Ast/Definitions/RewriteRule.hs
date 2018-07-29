module Kit.Ast.Definitions.RewriteRule where

import Control.Monad
import Kit.Ast.Definitions.FunctionDefinition
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Str

data RewriteRule a b = RewriteRule {
  ruleDoc :: Maybe Str,
  ruleMeta :: [Metadata],
  ruleModifiers :: [Modifier],
  ruleParams :: [TypeParam],
  ruleType :: b,
  rulePattern :: a,
  ruleBody :: Maybe a
} deriving (Eq, Show)
