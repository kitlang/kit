module Kit.Compiler.Passes.ExpandMacros where

import Control.Exception
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

type CompileFunc = CompileContext -> CCompiler -> IO ()
type MacroMap = HashTable TypePath (FunctionDefinition Expr (Maybe TypeSpec))

expandMacros
  :: CompileContext
  -> CCompiler
  -> CompileFunc
  -> [(Module, [SyntacticStatement])]
  -> IO [(Module, [SyntacticStatement])]
expandMacros ctx cc compile stmts = do
  case ctxMacro ctx of
    Just _  -> return stmts
    Nothing -> do
      macros <- findMacros stmts
      forMWithErrors stmts $ expandMacrosInMod ctx cc compile macros

findMacros :: [(Module, [SyntacticStatement])] -> IO MacroMap
findMacros stmts = do
  macros <- h_new
  forM_ stmts $ \(mod, stmts) -> forM_ stmts $ \s -> case stmt s of
    MacroDeclaration f ->
      let tp = (modPath mod, tpName $ functionName f)
      in  h_insert macros tp $ f { functionName = tp }
    _ -> return ()
  return macros

expandMacrosInMod ctx cc compile macros (mod, stmts) = do
  results <- forM stmts $ expandMacrosInStmt ctx cc compile mod macros
  return (mod, foldr (++) [] results)

expandMacrosInStmt
  :: CompileContext
  -> CCompiler
  -> CompileFunc
  -> Module
  -> MacroMap
  -> SyntacticStatement
  -> IO [SyntacticStatement]
expandMacrosInStmt ctx cc compile mod macros s = case stmt s of
  MacroCall name args -> do
    macro     <- findMacro ctx mod macros name (stmtPos s)
    expansion <- expandMacro ctx cc compile mod macro args
    recurse   <- forM expansion $ expandMacrosInStmt ctx cc compile mod macros
    let results = foldr (++) [] recurse
    results <- forM results $ addStmtToModuleInterface ctx mod
    return $ foldr (++) [] results
  _ -> return [s]

findMacro
  :: CompileContext
  -> Module
  -> MacroMap
  -> TypePath
  -> Span
  -> IO (FunctionDefinition Expr (Maybe TypeSpec))
findMacro ctx mod macros name pos = do
  result <- case name of
    ([], s) -> do
      imports <- getModImports ctx mod
      foldM
        (\acc modPath -> case acc of
          Just _  -> return acc
          Nothing -> h_lookup macros (modPath, s)
        )
        Nothing
        imports
    (m, s) -> h_lookup macros name
  case result of
    Just x  -> return x
    Nothing -> throwk $ BasicError
      ("Unknown macro: " ++ (s_unpack $ showTypePath name))
      (Just pos)

expandMacro
  :: CompileContext
  -> CCompiler
  -> (CompileContext -> CCompiler -> IO ())
  -> Module
  -> FunctionDefinition Expr (Maybe TypeSpec)
  -> [Expr]
  -> IO [SyntacticStatement]
expandMacro ctx cc compile mod def args = do
  printLogIf ctx $ "expanding macro: " ++ s_unpack
    (showTypePath $ functionName def)
  -- FIXME: also show args here
  result     <- newIORef ""
  -- FIXME: should be able to reuse already parsed modules
  -- FIXME: optimization: only compile a macro once, and run with each combination of args
  macroState <- newCtxState
  let macroCtx = ctx { ctxMainModule    = modPath mod
                     , ctxMacro         = Just (def, args)
                     , ctxRun           = True
                     , ctxResultHandler = Just $ writeIORef result
                     , ctxState         = macroState
                     , ctxVerbose       = ctxVerbose ctx - 1
                     , ctxBuildDir      = ctxBuildDir ctx </> "macro"
                     }
  compile macroCtx cc
  result <- readIORef result
  debugLog ctx result
  let parseResult =
        parseTokens
          $ scanTokens
              ("(macro " ++ s_unpack (showTypePath $ functionName def) ++ ")")
          $ s_pack result
  case parseResult of
    ParseResult r -> return r
    Err         e -> throw e
