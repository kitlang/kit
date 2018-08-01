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
  forM_
    defs
    (\(_, d) -> case d of
      DeclFunction f -> typeFunction ctx mod f
      DeclVar      v -> typeVar ctx mod v
      DeclTrait    t -> typeTrait ctx mod t
      DeclType     t -> typeTypeDefinition ctx mod t
      DeclRuleSet  _ -> return ()
    )
  when (ctxDumpAst ctx) $ dumpModuleContent ctx mod

dumpModuleContent :: CompileContext -> Module -> IO ()
dumpModuleContent ctx mod = do
  putStrLn $ show mod
  defs <- readIORef (modTypedContents mod)
  forM_ defs (dumpModuleDecl ctx mod)
  putStrLn ""

dumpModuleDecl ctx mod decl = do
  case decl of
    DeclFunction f -> do
      putStr $ "  function " ++ (s_unpack $ functionName f) ++ ": "
      if null $ functionArgs f
        then putStr "() -> "
        else do
          putStrLn "("
          forM_
            (functionArgs f)
            (\arg -> do
              t <- dumpCt ctx (argType arg)
              putStrLn $ "      " ++ s_unpack (argName arg) ++ ": " ++ t
            )
          putStr $ "    ) -> "
      rt <- dumpCt ctx (functionType f)
      putStrLn rt
      case functionBody f of
        Just x -> do
          out <- dumpAst ctx 2 x
          putStrLn out
        _ -> return ()
      putStrLn ""

    DeclVar v -> do
      t <- dumpCt ctx (varType v)
      putStrLn $ "  var " ++ (s_unpack $ varName v) ++ ": " ++ t ++ "\n"
      case varDefault v of
        Just x -> do
          out <- dumpAst ctx 2 x
          putStrLn out
        _ -> return ()

    DeclType t -> do
      putStrLn
        $  "  type "
        ++ (s_unpack $ showTypePath (modPath mod, typeName t))
      forM_
        (typeStaticFields t)
        (\v -> do
          t <- dumpCt ctx (varType v)
          putStrLn $ "    static var " ++ (s_unpack $ varName v) ++ ": " ++ t
          case varDefault v of
            Just x -> do
              out <- dumpAst ctx 3 x
              putStrLn out
            _ -> return ()
        )
      putStrLn ""

    _ -> return ()
