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
typeContent :: CompileContext -> IO ()
typeContent ctx = do
  mods <- ctxSourceModules ctx
  forM_ mods (typeModuleContent ctx)

typeModuleContent :: CompileContext -> Module -> IO ()
typeModuleContent ctx mod = do
  defs <- h_toList (modContents mod)
  tctx <- modTypeContext ctx mod
  -- FIXME: pos
  let varConverter = converter (convertExpr ctx tctx mod)
                               (resolveMaybeType ctx tctx mod NoPos)
  -- TODO: params
  let paramConverter params = varConverter
  converted <- forM
    defs
    (\(_, d) ->
      let r f x = do
            x' <- x
            return $ f x'
      in
        case d of
          DeclFunction f ->
            r DeclFunction $ convertFunctionDefinition paramConverter f
          DeclVar     v  -> r DeclVar $ convertVarDefinition varConverter v
          DeclType    t  -> r DeclType $ convertTypeDefinition paramConverter t
          DeclTrait t -> r DeclTrait $ convertTraitDefinition paramConverter t
          DeclRuleSet rs -> r DeclRuleSet $ convertRuleSet varConverter rs
    )
  iterativeTyping ctx mod converted 0
  -- addSpecializations ctx mod
  -- retype ctx mod
  when (ctxDumpAst ctx) $ dumpModuleContent ctx mod

iterativeTyping ctx mod converted passes = do
  when (passes > ctxRecursionLimit ctx) $ if ctxDumpAst ctx
    then do
      throwk $ BasicError
        ("Maximum number of compile passes exceeded while typing " ++ show mod)
        Nothing
      dumpModuleContent ctx mod
    else throwk $ BasicError
      (  "Maximum number of compile passes exceeded while typing "
      ++ show mod
      ++ "; run again with --dump-ast to see typed output"
      )
      Nothing
  results <- forM
    converted
    (\d -> do
      case d of
        DeclFunction f -> typeFunction ctx mod f
        DeclVar v -> typeVar ctx mod v
        DeclType t -> typeTypeDefinition ctx mod t
        DeclTrait t -> typeTrait ctx mod t
        DeclRuleSet rs -> return (Nothing, True)
    )
  let incomplete = filter (\(decl, complete) -> not complete) results
  when (not $ null incomplete)
    $ iterativeTyping ctx mod (catMaybes $ map fst incomplete) passes

dumpModuleContent :: CompileContext -> Module -> IO ()
dumpModuleContent ctx mod = do
  putStrLn $ show mod
  defs <- readIORef (modTypedContents mod)
  forM_ defs (dumpModuleDecl ctx mod)
  putStrLn ""
