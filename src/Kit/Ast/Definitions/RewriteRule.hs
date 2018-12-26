{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.Definitions.RewriteRule (
  RewriteRule (..),
  newRewriteRule,
  convertRewriteRule,
  RuleSet (..),
  newRuleSet,
  convertRuleSet
) where

import Control.Monad
import Data.Hashable
import GHC.Generics
import Kit.Ast.Definitions.Base
import Kit.Ast.TypePath
import Kit.Ast.Span
import Kit.Str

data RewriteRule a b = RewriteRule {
  rulePattern :: a,
  ruleBody :: Maybe a,
  rulePos :: Span,
  ruleThis :: Maybe a
} deriving (Eq, Generic, Show)

instance Positioned (RewriteRule a b) where
  position = rulePos

instance (Hashable a, Hashable b) => Hashable (RewriteRule a b)

newRewriteRule = RewriteRule
  { rulePattern = undefined
  , ruleBody    = Nothing
  , rulePos     = NoPos
  , ruleThis    = Nothing
  }

convertRewriteRule
  :: (Monad m) => Converter m a b c d -> RewriteRule a b -> m (RewriteRule c d)
convertRewriteRule converter r = do
  pattern  <- (exprConverter converter) $ rulePattern r
  body     <- maybeConvert (exprConverter converter) $ ruleBody r
  this     <- maybeConvert (exprConverter converter) $ ruleThis r
  return $ (newRewriteRule) { rulePattern = pattern
                            , ruleBody    = body
                            , rulePos     = rulePos r
                            , ruleThis    = this
                            }

data RuleSet a b = RuleSet {
  ruleSetName :: TypePath,
  ruleSetPos :: Span,
  ruleSetRules :: [RewriteRule a b]
} deriving (Eq, Show)

instance Positioned (RuleSet a b) where
  position = ruleSetPos

newRuleSet = RuleSet
  { ruleSetName  = undefined
  , ruleSetPos   = NoPos
  , ruleSetRules = []
  }

convertRuleSet
  :: (Monad m) => Converter m a b c d -> RuleSet a b -> m (RuleSet c d)
convertRuleSet converter r = do
  rules <- forM (ruleSetRules r) $ convertRewriteRule converter
  return $ (newRuleSet) { ruleSetName  = ruleSetName r
                        , ruleSetPos   = ruleSetPos r
                        , ruleSetRules = rules
                        }
