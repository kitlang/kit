module Kit.Compiler.Passes.GenerateIr where

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
  import Kit.Ir
  import Kit.Parser
  import Kit.Str

  generateIr :: CompileContext -> IO ()
  generateIr ctx = do
    return ()
