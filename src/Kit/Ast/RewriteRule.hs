module Kit.Ast.RewriteRule where

  import Kit.Ast.Expr
  import Kit.Ast.Modifier
  import Kit.Ast.TypeSpec
  import Kit.Str

  data TermRewriteRule = TermRewriteRule {
    rule_doc :: Maybe Str,
    rule_meta :: [Metadata],
    rule_modifiers :: [Modifier],
    rule_params :: [TypeParam],
    rule_type :: Maybe TypeSpec,
    rule_pattern :: Expr,
    rule_body :: Maybe Expr
  } deriving (Eq, Show)
