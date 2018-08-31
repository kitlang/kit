module Kit.Compiler (
  tryCompile,
  module Kit.Compiler.Binding,
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
import System.Process
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.DumpAst
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
  {-
    Load the main module and all of its dependencies recursively. Also builds
    module interfaces, which declare the set of types that exist in a module
    and map them to type variables.
  -}
  printLog "parsing and building module graph"
  declarations <- buildModuleGraph ctx

  {-
    Generate C modules for all includes found during buildModuleGraph.
  -}
  printLog "processing C includes"
  includeCModules ctx

  {-
    This step utilizes the module interfaces from buildModuleGraph to convert
    syntactic types to preliminary typed AST. Type annotations will be looked
    up and will fail if they don't resolve, but program semantics won't be
    checked yet; we'll get typed AST with a lot of spurious type variables,
    which will be unified later.
  -}
  printLog "resolving module types"
  resolved <- resolveModuleTypes ctx declarations

  {-
    TODO: expand procedural macros here
  -}

  {-
    Main checking of program semantics happens here. Takes and returns typed
    AST, but the return value should have all necessary type information. This
    step is iterative and repeats until successful convergence, or throws an
    exception on failure.
  -}
  printLog "typing module content"
  typedContent <- typeContent ctx resolved

  {-
    Any remaining unresolved type variables will be specialized here if
    possible. After this point, attempting to use an unresolved type variable
    will be an error.
  -}
  printLog "specializing types"
  specializeTypes ctx

  {-
    Generate declarations for observed generic usages.
  -}
  printLog "generating monomorphs"
  monomorphs <- generateMonomorphs ctx
  let typed =
        [ (fst $ head x, foldr (++) [] (map snd x))
        | x <- groupBy
          (\a b -> modPath (fst a) == modPath (fst b))
          ( sortBy (\a b -> compare (modPath $ fst a) (modPath $ fst b))
          $ (typedContent ++ monomorphs)
          )
        , not $ null x
        ]

  when (ctxDumpAst ctx) $ do
    printLog "typed AST:"
    forM_ typed (\(mod, decls) -> dumpModuleContent ctx mod decls)

  {-
    Convert typed AST to IR.
  -}
  printLog "generating intermediate representation"
  ir <- generateIr ctx typed

  {-
    Generate header and code files from IR.
  -}
  printLog "generating code"
  generated <- generateCode ctx ir

  {-
    Compile the generated code.
  -}
  binPath <- if ctxNoCompile ctx
    then do
      printLog "skipping compile"
      return Nothing
    else do
      printLog "compiling"
      compileCode ctx generated

  printLog "finished"

  when (ctxRun ctx) $ case binPath of
    Just x -> do
      callProcess x []
      return ()
    Nothing -> logMsg
      Nothing
      "--run was set, but no binary path was generated; skipping"
