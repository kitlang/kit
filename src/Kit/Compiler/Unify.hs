module Kit.Compiler.Unify where

  import Kit.Ast.Type
  import Kit.Compiler.Typer

  -- TODO
  -- Check whether type a unifies with b; i.e., can a value of type A be
  -- assigned to a variable of type B?
  unify :: ConcreteType -> ConcreteType -> (Bool, Maybe Substitution)
  unify a b = (True, Nothing)
