module Kit.Compiler.Unify where

  import Kit.Ast
  import Kit.Compiler.Typer

  -- TODO
  -- Check whether type a unifies with b; i.e., can a value of type A be
  -- assigned to a variable of type B?
  unify :: TypeSpec -> TypeSpec -> (Bool, Maybe Substitution)
  unify a b = (True, Nothing)
