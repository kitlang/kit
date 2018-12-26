module Kit.Compiler.Passes.GenerateCode (generateCode) where

import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.IO
import Text.PrettyPrint
import Kit.Ast
import Kit.Compiler.Generators.C
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Ir

{-
  Generates C code.

  Declarations are bundled into compilation units; to make dependency
  analysis and cross-dependencies easier, a single header is generated for the
  entire project and included from all c files. The header will contain type
  definitions and variable/function declarations.
-}
generateCode :: CompileContext -> [(Module, [IrBundle])] -> IO [TypePath]
generateCode ctx ir = do
  forM_ [libDir ctx, includeDir ctx, objDir ctx] $ \d -> do
    exists <- doesDirectoryExist d
    when exists $ removeDirectoryRecursive d
  generateProjectHeader ctx ir
  names <- forM ir $ generateModule ctx
  return $ catMaybes $ foldr (++) [] names
