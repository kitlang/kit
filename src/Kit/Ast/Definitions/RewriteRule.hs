module Kit.Ast.Definitions.RewriteRule where

import Control.Monad
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Parser.Span
import Kit.Str

data RewriteRule a b = RewriteRule {
  ruleDoc :: Maybe Str,
  ruleType :: b,
  rulePattern :: a,
  ruleBody :: Maybe a,
  rulePos :: Span
} deriving (Eq, Show)

newRewriteRule = RewriteRule {
  ruleDoc = Nothing,
  ruleType = undefined,
  rulePattern = undefined,
  ruleBody = Nothing,
  rulePos = NoPos
}

data RuleSet a b = RuleSet {
  ruleSetName :: Str,
  ruleSetPos :: Span,
  ruleSetDoc :: Maybe Str,
  ruleSetRules :: [RewriteRule a b]
} deriving (Eq, Show)

newRuleSet = RuleSet {
  ruleSetName = undefined,
  ruleSetPos = NoPos,
  ruleSetDoc = Nothing,
  ruleSetRules = []
}
