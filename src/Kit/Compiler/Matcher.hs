module Kit.Compiler.Matcher where

import Kit.Ast.Definitions
import Kit.Ast.Expr
import Kit.Ast.Identifier
import Kit.Ast.Statement

-- TODO: optimize
type RewriteRules a b = [RewriteRuleType a b]

match :: Expr -> Maybe [(Identifier, Expr)]
-- TODO
match x = Nothing

rewrite :: RewriteRules a b -> a -> a
rewrite (h : t) x = rewrite t (rewrite1 h x)
rewrite []      x = x

rewrite1 :: RewriteRuleType a b -> a -> a
-- TODO
rewrite1 r x = x
