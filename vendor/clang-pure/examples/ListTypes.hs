{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import           Language.C.Clang
import           Language.C.Clang.Cursor.Typed
import           Control.Lens
import qualified Data.ByteString.Char8 as BS
import           System.Environment
import           Data.Monoid ((<>))

main :: IO ()
main = do
  args <- getArgs
  case args of
    path : clangArgs -> do
      idx <- createIndex
      tu <- parseTranslationUnit idx "clang" path clangArgs
      let funDecs =
              cursorDescendantsF                                    -- fold over cursors recursively
            . folding (matchKind @'FunctionDecl)                    -- find only FunctionDecls...
            . filtered (isFromMainFile . rangeStart . cursorExtent) -- ...that are actually in the given file
            . to (\funDec -> cursorSpelling funDec <> " :: " <> typeSpelling (cursorType funDec))
      BS.putStrLn $ BS.unlines (translationUnitCursor tu ^.. funDecs)

    _ -> putStrLn "usage: list-fun-types path [clang opts]"
