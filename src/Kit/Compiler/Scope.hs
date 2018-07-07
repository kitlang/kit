module Kit.Compiler.Scope where

  import Data.IORef
  import Kit.Ast
  import Kit.HashTable
  import Kit.Str

  data Scope a = Scope {
    scopeBindings :: HashTable Str a
  }

  -- Create a new scope.
  newScope :: IO (Scope a)
  newScope = do
    bindings <- h_new
    return $ Scope {
      scopeBindings = bindings
    }

  -- Add a new binding to this scope.
  bindToScope :: Scope a -> Str -> a -> IO ()
  bindToScope scope s binding = h_insert (scopeBindings scope) s binding

  -- Look up a binding in this scope.
  resolveLocal :: Scope a -> Str -> IO (Maybe a)
  resolveLocal scope s = h_lookup (scopeBindings scope) s

  {-
    Look up a binding in a set of scopes. Prefers scopes earlier in the list.
  -}
  resolveBinding :: [Scope a] -> Str -> IO (Maybe a)
  resolveBinding (scope:scopes) s = do
    x <- resolveLocal scope s
    case x of
      Just _ -> return x
      Nothing -> resolveBinding scopes s
  resolveBinding [] s = do return Nothing

  -- Returns the list of bindings this scope contains.
  bindingList :: Scope a -> IO [a]
  bindingList s = do
    bindings <- h_toList $ scopeBindings s
    return $ map snd bindings
