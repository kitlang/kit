module Kit.HashTable where

import Data.Hashable
import qualified Data.HashTable.IO as H

{-
  This typedef + methods are here to make it easier to swap out hash table
  implementations if necessary.
-}
type HashTable k v = H.CuckooHashTable k v

-- defaulting a bit larger so we don't have to resize as frequently
h_new :: (Eq k, Hashable k) => IO (HashTable k v)
h_new = H.newSized 32

h_newSized :: (Eq k, Hashable k) => Int -> IO (HashTable k v)
h_newSized n = H.newSized n

h_insert :: (Eq k, Hashable k) => HashTable k v -> k -> v -> IO ()
h_insert = H.insert

h_delete :: (Eq k, Hashable k) => HashTable k v -> k -> IO ()
h_delete = H.delete

h_lookup :: (Eq k, Hashable k) => HashTable k v -> k -> IO (Maybe v)
h_lookup = H.lookup

h_exists :: (Eq k, Hashable k) => HashTable k v -> k -> IO (Bool)
h_exists m k = do
  val <- h_lookup m k
  return $ case val of
    Just _  -> True
    Nothing -> False

h_get :: (Eq k, Hashable k, Show k) => HashTable k v -> k -> IO v
h_get m k = do
  val <- h_lookup m k
  return $ case val of
    Just x  -> x
    Nothing -> error ("Unexpected missing HashTable key: " ++ show k)

h_toList :: (Eq k, Hashable k) => HashTable k v -> IO [(k, v)]
h_toList = H.toList
