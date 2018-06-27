module Kit.Compiler.Scope where

  import Kit.Ast
  import Kit.HashTable
  import Kit.Str

  data Scope = Scope {
    scope_bindings :: HashTable Str Binding
  } deriving (Show)

  newScope :: IO Scope
  newScope = do
    bindings <- h_new
    return $ Scope {
      scope_bindings = bindings
    }

  bindToScope scope s binding = h_insert (scope_bindings scope) s binding
