module Kit.Compiler.Scope where

import Data.IORef
import Kit.Ast
import Kit.HashTable
import Kit.Str

data Scope a = Scope {
  scopeNamespace :: [Str],
  -- bound names in this scope
  scopeBindings :: HashTable Str a,
  -- child namespaces within this scope
  subScopes :: HashTable Str (Scope a)
}

-- Create a new scope.
newScope :: [Str] -> IO (Scope a)
newScope n = do
  bindings  <- h_new
  subScopes <- h_new
  return $ Scope
    { scopeNamespace = n
    , scopeBindings  = bindings
    , subScopes      = subScopes
    }

getSubScope :: Scope a -> [Str] -> IO (Scope a)
getSubScope scope []      = return scope
getSubScope scope (h : t) = do
  existing <- h_lookup (subScopes scope) h
  case existing of
    Just x  -> getSubScope x t
    Nothing -> do
      x <- newScope (scopeNamespace scope ++ [h])
      h_insert (subScopes scope) h x
      getSubScope x t

-- Add a new binding to this scope.
bindToScope :: Scope a -> Str -> a -> IO ()
bindToScope scope s binding = h_insert (scopeBindings scope) s binding

bindSub :: Scope a -> [Str] -> Str -> a -> IO ()
bindSub scope namespace s binding = do
  subScope <- getSubScope scope namespace
  bindToScope subScope s binding

-- Look up a binding in this scope.
resolveLocal :: Scope a -> Str -> IO (Maybe a)
resolveLocal scope s = h_lookup (scopeBindings scope) s

scopeHas :: Scope a -> Str -> IO Bool
scopeHas scope s = h_exists (scopeBindings scope) s

scopeGet :: Scope a -> Str -> IO a
scopeGet scope s = h_get (scopeBindings scope) s

{-
  Look up a binding in a set of scopes. Prefers scopes earlier in the list.
-}
resolveBinding :: [Scope a] -> Str -> IO (Maybe a)
resolveBinding (scope : scopes) s = do
  x <- resolveLocal scope s
  case x of
    Just _  -> return x
    Nothing -> resolveBinding scopes s
resolveBinding [] s = do
  return Nothing

resolveBindingScope :: [(b, Scope a)] -> Str -> IO (Maybe b)
resolveBindingScope ((b, scope) : scopes) s = do
  x <- resolveLocal scope s
  case x of
    Just _  -> return (Just b)
    Nothing -> resolveBindingScope scopes s
resolveBindingScope [] s = do
  return Nothing

-- Returns the list of bindings this scope contains.
bindingList :: Scope a -> IO [a]
bindingList s = do
  bindings <- h_toList $ scopeBindings s
  return $ map snd bindings


-- Returns the list of bindings this scope contains.
bindingNames :: Scope a -> IO [Str]
bindingNames s = do
  bindings <- h_toList $ scopeBindings s
  return $ map fst bindings
