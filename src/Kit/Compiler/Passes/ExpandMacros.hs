module Kit.Compiler.Passes.ExpandMacros where

import Control.Monad
import Data.IORef
import Data.List
import System.FilePath
import Language.C
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.System.GCC
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.CCompiler
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypedExpr
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser
import Kit.Str

{-
  For each C header discovered during the BuildModuleGraph pass, parse the
  header to discover all declarations, and make these available from Kit.
-}
expandMacros :: CompileContext -> CCompiler -> IO ()
expandMacros ctx cc = do return ()
