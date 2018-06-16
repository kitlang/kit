module Kit.Compiler.Typer where

  import Kit.Str
  import Kit.Ast.Expr
  import Kit.Ast.Type

  data TypeEnvironment = TypeEnvironment {
    bindings :: [(Str, ConcreteType)],
    next_type_var :: TypeVarId
  } deriving (Eq, Show)

  type Substitution = (ConcreteType, ConcreteType)

  -- TODO
  -- Given a type environment, infer the type of an expression
  infer :: TypeEnvironment -> Expr -> [Substitution]
  infer t x = []
