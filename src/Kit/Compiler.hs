module Kit.Compiler (
  tryCompile,
  module Kit.Compiler.Context,
  module Kit.Compiler.Module,
  module Kit.Compiler.Passes,
  module Kit.Compiler.Scope,
  module Kit.Compiler.TypeUsage
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
  import Kit.Compiler.TypeUsage
  import Kit.Compiler.Utils
  import Kit.Error
  import Kit.HashTable
  import Kit.Log
  import Kit.Parser
  import Kit.Str

  tryCompile :: CompileContext -> IO (Either Errors ())
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
    printLog "typing expressions"
    typeExpressions ctx
    printLog "generating types"
    generateTypes ctx
    printLog "generating code"
    generateCode ctx
    printLog "finished"
