module Kit.Compiler.Passes.ExpandMacros (expandMacros) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Mutable
import Data.List
import System.Directory
import System.FilePath
import System.Process
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

data MacroData = MacroData {
  macroDef :: FunctionDefinition Expr (Maybe TypeSpec),
  macroTypePath :: TypePath,
  macroArgCount :: IORef Int,
  macroArgs :: HashTable [Expr] Int,
  macroResults :: HashTable Int [SyntacticStatement]
}

type CompileFunc = CompileContext -> CCompiler -> IO ()
type MacroMap = HashTable TypePath (FunctionDefinition Expr (Maybe TypeSpec))
type MacroInvocationMap = HashTable TypePath MacroData
type ModuleStatements = (Module, [SyntacticStatement])

expandMacros
  :: CompileContext
  -> CCompiler
  -> CompileFunc
  -> [ModuleStatements]
  -> IO [ModuleStatements]
expandMacros ctx cc compile stmts = do
  case ctxMacro ctx of
    Just _ -> return stmts
    Nothing ->
      if (any
            (\(mod, stmts) -> any
              (\s -> case stmt s of
                MacroCall _ _ -> True
                _             -> False
              )
              stmts
            )
            stmts
         )
        then do
          macros      <- findMacros stmts
          invocations <- findInvocations ctx macros stmts
          callMacros ctx cc compile invocations
          stmts <- forMWithErrors stmts
            $ expandMacrosInMod ctx macros invocations
          expandMacros ctx cc compile stmts
        else return stmts

findMacros :: [ModuleStatements] -> IO MacroMap
findMacros stmts = do
  macros <- h_new
  forM_ stmts $ \(mod, stmts) -> forM_ stmts $ \s -> case stmt s of
    MacroDeclaration f ->
      let tp = (modPath mod, tpName $ functionName f)
      in  h_insert macros tp $ f { functionName = tp }
    _ -> return ()
  return macros

findInvocations
  :: CompileContext -> MacroMap -> [ModuleStatements] -> IO MacroInvocationMap
findInvocations ctx macros stmts = do
  invocations <- h_new
  forM_ stmts $ \(mod, stmts) -> forM_ stmts $ \s -> case stmt s of
    MacroCall tp args -> do
      macro          <- findMacro ctx mod macros tp (stmtPos s)
      tp             <- return $ functionName macro
      invocationData <- h_lookup invocations tp
      invocationData <- case invocationData of
        Just x  -> return x
        Nothing -> do
          count   <- newRef 0
          args    <- h_new
          results <- h_new
          let invocation = MacroData
                { macroDef      = macro
                , macroTypePath = tp
                , macroArgCount = count
                , macroArgs     = args
                , macroResults  = results
                }
          h_insert invocations tp invocation
          return invocation
      existing <- h_lookup (macroArgs invocationData) args
      case existing of
        Just _  -> return ()
        Nothing -> do
          modifyRef (macroArgCount invocationData) ((+) 1)
          index <- readRef (macroArgCount invocationData)
          h_insert (macroArgs invocationData) args index
    _ -> return ()
  return invocations

callMacros ctx cc compile invocations = do
  h_mapM_
    (\(tp, invocation) -> do
      let def = macroDef invocation
      printLogIf ctx $ "expanding macro: " ++ s_unpack
        (showTypePath $ functionName def)
      result     <- (newRef "") :: IO (IORef String)
      -- FIXME: should be able to reuse already parsed modules
      macroState <- newCtxState
      args       <- h_toList $ macroArgs invocation
      let mod = tpNamespace $ functionName def
      let buildDir =
            ctxBuildDir ctx
              </>  "macro"
              </>  (moduleFilePath mod)
              -<.> ""
              </>  (s_unpack $ tpName $ functionName def)
      let macroCtx = ctx { ctxMainModule = mod
                         , ctxMacro = Just (def, [ (a, b) | (b, a) <- args ])
                         , ctxRun        = False
                         , ctxState      = macroState
                         , ctxVerbose    = ctxVerbose ctx - 1
                         , ctxBuildDir   = buildDir
                         , ctxOutputPath = buildDir </> "macro"
                         }
      compile macroCtx cc
      let outName = ctxOutputPath macroCtx
      binPath <- canonicalizePath outName
      forM_ args $ \(_, index) -> do
        result <- readCreateProcess (proc binPath [show index]) ""
        debugLog ctx result
        let
          parseResult =
            parseTokens
              $ scanTokens
                  (  "(macro "
                  ++ s_unpack (showTypePath $ functionName def)
                  ++ ")"
                  )
              $ B.pack result
        case parseResult of
          ParseResult r -> h_insert (macroResults invocation) index r
          Err         e -> throw e
    )
    invocations

expandMacrosInMod
  :: CompileContext
  -> MacroMap
  -> MacroInvocationMap
  -> ModuleStatements
  -> IO ModuleStatements
expandMacrosInMod ctx macros invocations (mod, stmts) = do
  results <- forM stmts $ expandMacrosInStmt ctx mod macros invocations
  return (mod, foldr (++) [] results)

expandMacrosInStmt
  :: CompileContext
  -> Module
  -> MacroMap
  -> MacroInvocationMap
  -> SyntacticStatement
  -> IO [SyntacticStatement]
expandMacrosInStmt ctx mod macros invocations s = case stmt s of
  MacroCall tp args -> do
    macro      <- findMacro ctx mod macros tp $ stmtPos s
    tp         <- return $ functionName macro
    invocation <- h_get invocations tp
    index      <- h_get (macroArgs invocation) args
    result     <- h_get (macroResults invocation) index
    results    <- forM result $ addStmtToModuleInterface ctx mod
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
      foldM
        (\acc modPath -> case acc of
          Just _  -> return acc
          Nothing -> h_lookup macros (modPath, s)
        )
        Nothing
        (modImportPaths mod)
    (m, s) -> h_lookup macros name
  case result of
    Just x  -> return x
    Nothing -> throwk $ BasicError
      ("Unknown macro: " ++ (s_unpack $ showTypePath name))
      (Just pos)
