module Kit.Compiler.Passes.GenerateCode where

  import Control.Exception
  import Control.Monad
  import Data.IORef
  import Data.List
  import System.Directory
  import System.FilePath
  import Kit.Ast
  import Kit.Compiler.Context
  import Kit.Compiler.Module
  import Kit.Compiler.Utils
  import Kit.Error
  import Kit.HashTable
  import Kit.Parser
  import Kit.Str

  generateCode :: CompileContext -> IO ()
  generateCode ctx = do
    mods <- h_toList $ ctxModules ctx
    forM_ (map snd mods) (generateModule ctx)
    return ()

  generateModule :: CompileContext -> Module -> IO ()
  generateModule ctx mod = do
    createDirectoryIfMissing True $ takeDirectory $ includePath ctx (mod_path mod)
    createDirectoryIfMissing True $ takeDirectory $ libPath ctx (mod_path mod)
    return ()

  includePath :: CompileContext -> ModulePath -> FilePath
  includePath ctx mod = ((ctxOutputDir ctx) </> "include" </> (moduleFilePath mod -<.> ".h"))

  libPath :: CompileContext -> ModulePath -> FilePath
  libPath ctx mod = ((ctxOutputDir ctx) </> "lib" </> (moduleFilePath mod -<.> ".c"))
