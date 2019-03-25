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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.C.Clang.Location
  ( SourceRange()
  , rangeStart, rangeEnd
  , SourceLocation()
  , spellingLocation
  , isInSystemHeader
  , isFromMainFile
  , Location(..)
  )
where

import Language.C.Clang.Internal.FFI
import Language.C.Clang.Internal.Types

deriving instance Eq Location
deriving instance Show Location