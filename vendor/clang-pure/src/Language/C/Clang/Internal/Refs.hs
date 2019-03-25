{-
Copyright 2014 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Language.C.Clang.Internal.Refs where

import Control.Concurrent.MVar
import GHC.ForeignPtr (unsafeForeignPtrToPtr)
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import System.IO.Unsafe

type Finalizer = IO ()

data NodeState = NodeState
  { destructable :: !Bool
  , refCount :: !Int
  }

type family RefOf n
class Clang n where
  deref :: n -> (Ptr (RefOf n) -> IO a) -> IO a
  unsafeToPtr :: n -> Ptr (RefOf n)

class Clang n => Parent n where
  incCount :: n -> IO ()
  decCount :: n -> IO ()

type family ParentOf n
class (Clang n, Parent (ParentOf n)) => Child n where
  parent :: n -> ParentOf n

data Root a = Root
  { nodePtr :: ForeignPtr a
  , nodeState :: MVar NodeState
  , trueFinalize :: Finalizer
  }

newRoot :: Ptr a -> Finalizer -> IO (Root a)
newRoot ptr fin = do
  nsv <- newMVar $ NodeState { destructable = False, refCount = 0 }
  nptr <- newForeignPtr ptr $ modifyMVar_ nsv $ \ns ->
    if refCount ns == 0
      then fin >> return ns
      else return $ ns { destructable = True }
  return $ Root nptr nsv fin

instance Parent (Root a) where
  incCount r = modifyMVar_ (nodeState r) $ \ns -> return ns { refCount = refCount ns + 1 }
  decCount r = modifyMVar_ (nodeState r) $ \ns -> do
    if refCount ns == 1 && destructable ns
      then trueFinalize r >> return ns { refCount = 0, destructable = False }
      else return ns { refCount = refCount ns - 1 }

type instance RefOf (Root a) = a
instance Clang (Root a) where
  deref r = withForeignPtr (nodePtr r)
  unsafeToPtr = unsafeForeignPtrToPtr . nodePtr

data Node p a = Node p (Root a)

newNode :: Parent p => p -> (Ptr (RefOf p) -> IO ( Ptr a, Finalizer )) -> IO (Node p a)
newNode prn f = deref prn $ \pptr -> do
  ( cptr, cfin ) <- f pptr
  incCount prn
  rt <- newRoot cptr (cfin >> decCount prn)
  return $ Node prn rt

instance Clang p => Parent (Node p a) where
  incCount (Node _ n) = incCount n
  decCount (Node _ n) = decCount n

type instance ParentOf (Node p a) = p
instance Parent p => Child (Node p a) where
  parent (Node p _) = p

type instance RefOf (Node p a) = a
instance Clang p => Clang (Node p a) where
  deref (Node p n) f = deref p $ \_ -> deref n f
  unsafeToPtr (Node _ n) = unsafeToPtr n

data Leaf p a = Leaf p (ForeignPtr a)

newLeaf :: Parent p => p -> (Ptr (RefOf p) -> IO ( Ptr a, Finalizer )) -> IO (Leaf p a)
newLeaf prn f = deref prn $ \pptr -> do
    ( cptr, cfin ) <- f pptr
    incCount prn
    rt <- newForeignPtr cptr (cfin >> decCount prn)
    return $ Leaf prn rt

type instance ParentOf (Leaf p a) = p
instance Parent p => Child (Leaf p a) where
  parent (Leaf p _) = p

type instance RefOf (Leaf p a) = a
instance Clang p => Clang (Leaf p a) where
  deref (Leaf p n) f = deref p $ \_ -> withForeignPtr n f
  unsafeToPtr (Leaf _ n) = unsafeForeignPtrToPtr n

pointerEq :: Clang n => n -> n -> Bool
pointerEq r r' = unsafeToPtr r == unsafeToPtr r'

pointerCompare :: Clang n => n -> n -> Ordering
pointerCompare r r' = unsafeToPtr r `compare` unsafeToPtr r'

uderef :: Clang r => r -> (Ptr (RefOf r) -> IO a) -> a
uderef r f = unsafePerformIO $ deref r f