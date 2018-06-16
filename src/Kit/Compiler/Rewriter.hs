module Kit.Compiler.Rewriter where

  import Kit.Ast.Expr

  -- TODO: optimize
  type RewriteRules = [RewriteRule]

  rewrite :: RewriteRules -> Expr -> Expr
  rewrite (h:t) x = rewrite t (rewrite1 h x)
  rewrite [] x = x

  rewrite1 :: RewriteRule -> Expr -> Expr
  -- TODO
  rewrite1 r x = x
