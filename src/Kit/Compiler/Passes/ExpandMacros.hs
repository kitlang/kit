module Kit.Compiler.Passes.ExpandMacros (expandMacros) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Mutable
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Language.C
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.System.GCC
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.Typers.ConvertExpr
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser
import Kit.Str
import Kit.Toolchain

type CompileFunc = CompileContext -> Toolchain -> Toolchain -> IO ()
type ModuleStatements = (Module, [SyntacticStatement])

expandMacros
  :: CompileContext
  -> Toolchain
  -> CompileFunc
  -> [ModuleStatements]
  -> IO [ModuleStatements]
expandMacros ctx cc compile stmts = do
  case ctxMacro ctx of
    Just _  -> return stmts
    Nothing -> do
      findMacros      ctx stmts
      findInvocations ctx stmts
      callMacros ctx cc compile
      results <- forMWithErrors stmts $ expandMacrosInMod ctx
      if or $ map snd results
        then expandMacros ctx cc compile $ map fst results
        else return $ map fst results

findMacros :: CompileContext -> [ModuleStatements] -> IO ()
findMacros ctx stmts = do
  let macros = ctxMacros ctx
  forM_ stmts $ \(mod, stmts) -> forM_ stmts $ \s -> case stmt s of
    MacroDeclaration f ->
      let tp = (modPath mod, tpName $ functionName f)
      in  h_insert macros tp $ f { functionName = tp }
    _ -> return ()

findInvocations :: CompileContext -> [ModuleStatements] -> IO ()
findInvocations ctx stmts = do
  let
    macros      = ctxMacros ctx
    invocations = ctxMacroInvocations ctx
    recordMacro mod tp args pos isStmt = do
      macro <- findMacro ctx mod tp pos
      macro <- case macro of
        Just x  -> return x
        Nothing -> throwk $ BasicError
          ("Unknown macro: " ++ (s_unpack $ showTypePath tp))
          (Just pos)
      tp             <- return $ functionName macro
      invocationData <- h_lookup invocations tp
      invocationData <- case invocationData of
        Just x  -> return x
        Nothing -> do
          count   <- newRef 0
          args    <- h_new
          targs   <- h_new
          results <- h_new
          let invocation = MacroData
                { macroDef       = macro
                , macroTypePath  = tp
                , macroArgCount  = count
                , macroArgs      = args
                , macroTypedArgs = targs
                , macroResults   = results
                }
          h_insert invocations tp invocation
          return invocation
      existing <- h_lookup (macroArgs invocationData) args
      case existing of
        Just _  -> return ()
        Nothing -> do
          modifyRef (macroArgCount invocationData) ((+) 1)
          index <- readRef (macroArgCount invocationData)
          tctx  <- newTypeContext []
          unless isStmt $ do
            targs <- mapM (convertExpr ctx tctx mod []) args
            h_insert (macroTypedArgs invocationData) targs args
          h_insert (macroArgs invocationData) args (index, pos, isStmt)
    recordMacrosInExpression mod e = exprMapReduce
      (\x -> case expr x of
        Call e _ args ->
          let f e acc = case expr e of
                Identifier (Var ([], n)) ->
                  Just (modulePathToTypePath (n : acc), args)
                Field x (Var ([], n)) -> f x (n : acc)
                _         -> Nothing
          in  f e []
        _ -> Nothing
      )
      (\x acc -> case x of
        Just (tp, args) -> do
          macro <- findMacro ctx mod tp $ pos e
          case macro of
            Just f ->
              recordMacro mod (functionName f) args (pos e) False >> acc
            _ -> acc
        Nothing -> acc
      )
      expr
      (return ())
      e
    recordMacrosInFunction mod f@(FunctionDefinition { functionBody = Just x })
      = recordMacrosInExpression mod x
    recordMacrosInFunction _ _ = return ()

  forM_ stmts $ \(mod, stmts) -> forM_ stmts $ \s -> case stmt s of
    MacroCall tp args     -> recordMacro mod tp args (stmtPos s) True
    FunctionDeclaration f -> recordMacrosInFunction mod f
    TypeDeclaration t ->
      mapM_ (recordMacrosInFunction mod)
        $  (typeMethods t)
        ++ (typeStaticMethods t)
    TraitDeclaration t ->
      mapM_ (recordMacrosInFunction mod)
        $  (traitMethods t)
        ++ (traitStaticMethods t)
    Implement i ->
      mapM_ (recordMacrosInFunction mod)
        $  (implMethods i)
        ++ (implStaticMethods i)
    ExtendDefinition _ defStmts -> forM_ defStmts $ \s -> case s of
      DefMethod f -> recordMacrosInFunction mod f
      _           -> return ()
    _ -> return ()

callMacros :: CompileContext -> Toolchain -> CompileFunc -> IO ()
callMacros ctx cc compile = do
  let invocations = ctxMacroInvocations ctx
  h_mapM_
    (\(tp, invocation) -> do
      let def = macroDef invocation
      printLogIf ctx $ "expanding macro: " ++ s_unpack
        (showTypePath $ functionName def)
      result     <- (newRef "") :: IO (IORef String)
      -- FIXME: should be able to reuse already parsed modules
      macroState <- newCtxState
      macroState <- return $ macroState
        { ctxStateMacros           = ctxMacros ctx
        , ctxStateMacroInvocations = ctxMacroInvocations ctx
        }
      args <- h_toList $ macroArgs invocation
      let mod = tpNamespace $ functionName def
      let buildDir =
            ctxBuildDir ctx
              </>  "macro"
              </>  (moduleFilePath mod)
              -<.> ""
              </>  (s_unpack $ tpName $ functionName def)
      let macroCtx = ctx
            { ctxMainModule = mod
            , ctxMacro      = Just (def, [ (a, b) | (b, (a, _, _)) <- args ])
            , ctxRun        = False
            , ctxState      = macroState
            , ctxVerbose    = ctxVerbose ctx - 1
            , ctxBuildDir   = buildDir
            , ctxOutputPath = buildDir </> "macro"
            }
      compile macroCtx cc cc
      let outName = ctxOutputPath macroCtx
      binPath <- canonicalizePath outName
      forM_ args $ \(_, (index, pos, isStatement)) -> do
        (exitcode, result, stderr) <- readCreateProcessWithExitCode
          (proc binPath [show index])
          ""
        case exitcode of
          ExitSuccess -> do
            let outPath =
                  buildDir </> ".results" ++ "." ++ (show index) ++ ".kit"
            writeFile outPath result
            when (not $ null stderr) $ traceLog stderr
            if isStatement
              then do
                let
                  parseResult =
                    parseTokens $ scanTokens (FileSpan outPath) $ B.pack result
                case parseResult of
                  ParseResult r ->
                    h_insert (macroResults invocation) index (MacroStatements r)
                  Err e -> throw e
              else do
                let parseResult =
                      parseExpr $ scanTokens (FileSpan outPath) $ B.pack result
                case parseResult of
                  ParseResult r ->
                    h_insert (macroResults invocation) index (MacroExpression r)
                  Err e -> throw e
          ExitFailure code -> do
            throwk $ BasicError
              (  "Macro "
              ++ s_unpack (showTypePath $ functionName def)
              ++ " failed (exit status "
              ++ show code
              ++ (if null stderr then "" else ("):\n\n" ++ stderr))
              )
              (Just pos)
    )
    invocations

expandMacrosInMod
  :: CompileContext -> ModuleStatements -> IO (ModuleStatements, Bool)
expandMacrosInMod ctx (mod, stmts) = do
  results <- forM stmts $ expandMacrosInStmt ctx mod
  return ((mod, foldr (++) [] $ map fst results), or $ map snd results)

expandMacrosInStmt
  :: CompileContext
  -> Module
  -> SyntacticStatement
  -> IO ([SyntacticStatement], Bool)
expandMacrosInStmt ctx mod s = case stmt s of
  MacroCall tp args -> do
    macro <- findMacro ctx mod tp $ stmtPos s
    macro <- case macro of
      Just x  -> return x
      Nothing -> throwk $ BasicError
        ("Unknown macro: " ++ (s_unpack $ showTypePath tp))
        (Just $ stmtPos s)
    tp                       <- return $ functionName macro
    invocation               <- h_get (ctxMacroInvocations ctx) tp
    (index, _, _)            <- h_get (macroArgs invocation) args
    (MacroStatements result) <- h_get (macroResults invocation) index
    results                  <- forM result $ addStmtToModuleInterface ctx mod
    return $ (foldr (++) [] results, True)
  _ -> return ([s], False)

findMacro
  :: CompileContext
  -> Module
  -> TypePath
  -> Span
  -> IO (Maybe (FunctionDefinition Expr TypeSpec))
findMacro ctx mod name pos = do
  case name of
    ([], s) -> do
      foldM
        (\acc modPath -> case acc of
          Just _  -> return acc
          Nothing -> h_lookup (ctxMacros ctx) (modPath, s)
        )
        Nothing
        (modImportPaths mod)
    (m, s) -> h_lookup (ctxMacros ctx) name
