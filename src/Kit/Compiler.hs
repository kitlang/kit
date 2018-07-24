module Kit.Compiler (
  tryCompile,
  module Kit.Compiler.Context,
  module Kit.Compiler.Module,
  module Kit.Compiler.Passes,
  module Kit.Compiler.Scope,
  module Kit.Compiler.TypeContext,
  module Kit.Compiler.Unify
) where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import System.Directory
import System.FilePath
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Passes
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser
import Kit.Str

tryCompile :: CompileContext -> IO (Either KitError ())
tryCompile context = try $ compile context

{-
  Run compilation to completion from the given CompileContext. Throws an
  Error on failure.
-}
compile :: CompileContext -> IO ()
compile ctx = do
  debugLog ctx $ show ctx
  -- load the main module and all of its dependencies recursively
  printLog "building module graph"
  buildModuleGraph ctx
  printLog "processing C includes"
  includeCModules ctx
  printLog "resolving module types"
  resolveModuleTypes ctx
  printLog "typing module content"
  typeContent ctx
  printLog "generating internal representation"
  generateIr ctx
  printLog "generating code"
  generateCode ctx
  if ctxNoCompile ctx
    then printLog "skipping compile"
    else do
      printLog "compiling"
      compileCode ctx
  printLog "finished"
