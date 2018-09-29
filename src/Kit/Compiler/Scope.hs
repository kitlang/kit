module Kit.Compiler.Scope where

import Data.IORef
import Kit.HashTable
import Kit.Str

data Scope a = Scope {
  -- bound names in this scope
  scopeBindings :: HashTable Str a,
  lastTmpVar :: IORef (Int)
}

-- Create a new scope.
newScope :: IO (Scope a)
newScope = do
  bindings <- h_new
  tmp      <- newIORef 0
  return $ Scope {scopeBindings = bindings, lastTmpVar = tmp}

-- Add a new binding to this scope.
bindToScope :: Scope a -> Str -> a -> IO ()
bindToScope scope s binding = h_insert (scopeBindings scope) s binding

-- Look up a binding in this scope.
resolveLocal :: Scope a -> Str -> IO (Maybe a)
resolveLocal scope s = h_lookup (scopeBindings scope) s

scopeHas :: Scope a -> Str -> IO Bool
scopeHas scope s = h_exists (scopeBindings scope) s

scopeGet :: Scope a -> Str -> IO a
scopeGet scope s = h_get (scopeBindings scope) s

makeTmpVar :: Scope a -> IO Str
makeTmpVar scope = do
  last <- readIORef (lastTmpVar scope)
  let next = last + 1
  writeIORef (lastTmpVar scope) next
  return $ s_concat ["__tmp", s_pack $ show next]

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
