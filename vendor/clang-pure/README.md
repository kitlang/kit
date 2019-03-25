# Pure Haskell bindings to [libclang](http://clang.llvm.org/doxygen/group__CINDEX.html)
[![Hackage](https://img.shields.io/hackage/v/clang-pure.svg)](http://hackage.haskell.org/package/clang-pure) [![Build Status](https://travis-ci.org/chpatrick/clang-pure.svg?branch=master)](https://travis-ci.org/chpatrick/clang-pure)

A Haskell library for pure C++ code analysis

## API examples

### Enumerate all function declarations in `main.cpp`

```haskell
module Main (main) where

import Control.Monad
import Language.C.Clang

main :: IO ()
main = do
    idx <- createIndex
    tu <- parseTranslationUnit idx "main.cpp" ["-I/usr/local/include"]
    let root = translationUnitCursor tu
        children = cursorChildren root
        functionDecls = filter (\c -> cursorKind c == FunctionDecl) children
    forM_ functionDecls (print . cursorSpelling)
```

### List all function declarations and their types, `lens`-style

```haskell
idx <- createIndex
tu <- parseTranslationUnit idx path clangArgs
let funDecs =
      -- fold over cursors recursively
        cursorDescendantsF
      -- finding FunctionDecls...
      . folding (matchKind @'FunctionDecl)
      -- ...that are actually in the given file
      . filtered (isFromMainFile . rangeStart . cursorExtent)
      -- and get their names and types
      . to (\funDec -> cursorSpelling funDec <> " :: " <> typeSpelling (cursorType funDec))
BS.putStrLn $ BS.unlines (translationUnitCursor tu ^.. funDecs)
```

## Development

[View development guide][building]

[building]: DEV.md
[libclang]: http://clang.llvm.org/doxygen/group__CINDEX.html
