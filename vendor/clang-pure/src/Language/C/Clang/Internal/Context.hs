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

module Language.C.Clang.Internal.Context where

import qualified Data.Map as M
import Data.Monoid ((<>))
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

import Language.C.Clang.Internal.Types

clangTypesTable :: M.Map C.TypeSpecifier TH.TypeQ
clangTypesTable = M.fromList
  [ (C.TypeName "CXIndex", [t| CXIndex |])
  , (C.TypeName "CXTranslationUnit", [t| CXTranslationUnit |])
  , (C.TypeName "CXCursor", [t| CXCursor |])
  , (C.TypeName "CXString", [t| CXString |])
  , (C.TypeName "CXSourceRange", [t| CXSourceRange |])
  , (C.TypeName "CXSourceLocation", [t| CXSourceLocation |])
  , (C.TypeName "CXFile", [t| CXFile |])
  , (C.TypeName "CXType", [t| CXType |])
  , (C.TypeName "CXToken", [t| CXToken |])
  ]

clangCtx :: C.Context
clangCtx = C.baseCtx <> C.funCtx <> C.vecCtx <> ctx
  where
    ctx = mempty { C.ctxTypesTable = clangTypesTable }
