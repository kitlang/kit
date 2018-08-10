module Kit.Compiler.Passes.TypeModuleContent where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import System.FilePath
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.DumpAst
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Parser
import Kit.Str

{-
  Performs type resolution and type checking; converts Expr into TypedExpr and
  (Maybe TypeSpec) annotations into ConcreteTypes.

  See Kit.Compiler.Typers.* for specific typing implementations.
-}
typeContent
  :: CompileContext -> [(Module, [TypedDecl])] -> IO [(Module, [TypedDecl])]
typeContent ctx modContent = do
  results <- typeIterative ctx modContent [] (ctxRecursionLimit ctx)
  when (ctxDumpAst ctx)
    $ forM_ results (\(mod, decls) -> dumpModuleContent ctx mod decls)
  return results

typeIterative
  :: CompileContext
  -> [(Module, [TypedDecl])]
  -> [(Module, [TypedDecl])]
  -> Int
  -> IO [(Module, [TypedDecl])]
typeIterative ctx input output limit = do
  when (limit <= 0) $ if ctxDumpAst ctx
    then do
      throwk $ BasicError
        ("Maximum number of compile passes exceeded while typing; incomplete content:"
        )
        Nothing
      mods <- ctxSourceModules ctx
      forM_
        input
        (\(mod, decls) -> do
          putStrLn $ show mod
          forM_ decls (dumpModuleDecl ctx mod 0)
          putStrLn ""
        )
    else throwk $ BasicError
      (  "Maximum number of compile passes exceeded while typing;"
      ++ "; run again with --dump-ast to see typed output"
      )
      Nothing
  results <- forM
    input
    (\(mod, decls) -> do
      results <- forM
        decls
        (\d -> do
          (x, complete) <- case d of
            DeclFunction f  -> typeFunction ctx mod f
            DeclVar      v  -> typeVar ctx mod v
            DeclType     t  -> typeTypeDefinition ctx mod t
            DeclTrait    t  -> typeTrait ctx mod t
            DeclRuleSet  rs -> return (Nothing, True)
          return (x, complete)
        )
      let (incompleteResults, completeResults) = foldr
            (\(x, complete) (i, c) ->
              if complete then (i, x : c) else (x : i, c)
            )
            ([], [])
            results
      return (mod, (incompleteResults, completeResults))
    )

  let incomplete = [ (mod, y) | (mod, x) <- results, let y = catMaybes $ fst x, not $ null y ]
  let complete   = [ (mod, y) | (mod, x) <- results, let y = catMaybes $ snd x, not $ null y ]

  if (null incomplete)
    then return complete
    else typeIterative ctx incomplete complete (limit - 1)

dumpModuleContent :: CompileContext -> Module -> [TypedDecl] -> IO ()
dumpModuleContent ctx mod defs = do
  putStrLn $ show mod
  forM_ defs (dumpModuleDecl ctx mod 0)
  putStrLn ""
