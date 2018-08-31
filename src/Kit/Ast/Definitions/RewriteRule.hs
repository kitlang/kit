module Kit.Ast.Definitions.RewriteRule where

import Control.Monad
import Kit.Ast.ConcreteType
import Kit.Ast.Definitions.Base
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypePath
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

newRewriteRule = RewriteRule
  { ruleDoc     = Nothing
  , ruleType    = undefined
  , rulePattern = undefined
  , ruleBody    = Nothing
  , rulePos     = NoPos
  }

-- convertRule
--   :: (Monad m) => Converter m a b c d -> RewriteRule a b -> m (RewriteRule c d)
-- convertRule converter r = do
--   ruleType <- (typeConverter converter) (rulePos r) (ruleType r)
--   pattern  <- (exprConverter converter) (rulePattern r)
--   body     <- maybeConvert (exprConverter converter) (ruleBody r)
--   return $ (newRewriteRule) { ruleDoc     = ruleDoc r
--                             , ruleType    = ruleType
--                             , rulePattern = pattern
--                             , ruleBody    = body
--                             , rulePos     = rulePos r
--                             }

data RuleSet a b = RuleSet {
  ruleSetName :: TypePath,
  ruleSetPos :: Span,
  ruleSetDoc :: Maybe Str,
  ruleSetThis :: Maybe ConcreteType,
  ruleSetRules :: [RewriteRule a b]
} deriving (Eq, Show)

newRuleSet = RuleSet
  { ruleSetName  = undefined
  , ruleSetPos   = NoPos
  , ruleSetDoc   = Nothing
  , ruleSetThis  = Nothing
  , ruleSetRules = []
  }


-- convertRuleSet
--   :: (Monad m) => Converter m a b c d -> RuleSet a b -> m (RuleSet c d)
-- convertRuleSet converter r = do
--   rules <- forM (ruleSetRules r) (convertRule converter)
--   return $ (newRuleSet) { ruleSetName  = ruleSetName r
--                         , ruleSetPos   = ruleSetPos r
--                         , ruleSetDoc   = ruleSetDoc r
--                         , ruleSetThis  = ruleSetThis r
--                         , ruleSetRules = rules
--                         }
