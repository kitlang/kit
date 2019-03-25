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

module Language.C.Clang (
  module C,
  -- * Index
  ClangIndex(),
  createIndex,
  -- * Utilities
  Clang(),
  ClangOrd(..)
  ) where

import Language.C.Clang.File as C
import Language.C.Clang.Location as C
import Language.C.Clang.Token as C
import Language.C.Clang.TranslationUnit as C
import Language.C.Clang.Type as C

import Language.C.Clang.Internal.FFI
import Language.C.Clang.Internal.Refs
import Language.C.Clang.Internal.Types

-- | The `Eq` instance for `Clang` types checks structural equality,
-- i.e. whether they represent the same object in the translation unit.
--
-- Wrapping values in this type provides `Eq` and `Ord` instances based on reference equality.
newtype ClangOrd a = ClangOrd { getOrdered :: a }

instance Clang a => Eq (ClangOrd a) where
  ClangOrd x == ClangOrd y = x `pointerEq` y

instance Clang a => Ord (ClangOrd a) where
  ClangOrd x `compare` ClangOrd y = x `pointerCompare` y