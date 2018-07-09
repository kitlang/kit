module Kit.Compiler.Matcher where

  import Kit.Ast.Expr
  import Kit.Ast.Lvalue
  import Kit.Ast.Statement

  -- TODO: optimize
  type RewriteRules = [RewriteRule]

  match :: Expr -> Maybe [(Lvalue, Expr)]
  -- TODO
  match x = Nothing

  rewrite :: RewriteRules -> Expr -> Expr
  rewrite (h:t) x = rewrite t (rewrite1 h x)
  rewrite [] x = x

  rewrite1 :: RewriteRule -> Expr -> Expr
  -- TODO
  rewrite1 r x = x
