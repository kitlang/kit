module Kit.Compiler (
  tryCompile,
  module Kit.Compiler.Context,
  module Kit.Compiler.Module,
  module Kit.Compiler.Scope
) where

  import Control.Exception
  import Control.Monad
  import Data.List
  import System.Directory
  import System.FilePath
  import Kit.Ast
  import Kit.Compiler.Context
  import Kit.Compiler.Module
  import Kit.Compiler.Scope
  import Kit.Error
  import Kit.HashTable
  import Kit.Log
  import Kit.Parser
  import Kit.Str

  tryCompile :: CompileContext -> IO (Either Errors ())
  tryCompile context = try $ compile context

  debugLog :: CompileContext -> String -> IO ()
  debugLog ctx msg = do
    if context_verbose ctx
      then logMsg Debug msg
      else return ()

  {-
    Run compilation to completion from the given CompileContext. Throws an
    Error on failure.
  -}
  compile :: CompileContext -> IO ()
  compile ctx = do
    debugLog ctx $ show ctx
    modules <- h_new
    preludes <- h_new
    let ctx' = ctx {context_modules = modules, context_preludes = preludes}
    -- load the main module and all of its dependencies recursively
    printLog "building module graph"
    mainModule <- loadModule ctx' (context_main_module ctx') Nothing
    printLog "scanning for type info"
    printLog "typing expressions"
    printLog "generating code"
    return ()

  loadModule :: CompileContext -> ModulePath -> Maybe Span -> IO Module
  loadModule ctx mod pos = do
    existing <- h_lookup (context_modules ctx) mod
    case existing of
      Just x -> return x
      Nothing -> do
        m <- _loadModule ctx mod pos
        h_insert (context_modules ctx) mod m
        debugLog ctx $ "module <" ++ s_unpack (showModulePath mod) ++ "> imports: " ++ (intercalate ", " (map (s_unpack . showModulePath . fst) $ mod_imports m))
        errs <- foldM (_loadImportedModule ctx) [] (mod_imports m)
        if errs == []
          then return m
          else throw $ Errs errs

  {-
    Find all relevant prelude modules for a package, and return a list of
    expressions to prepend to the module contents.

    Given module pkg1.pkg2.mymod, this searches for:

    - pkg1.pkg2.prelude
    - pkg1.prelude
    - prelude

    and appends the contents of any of these modules that exist in reverse
    order.
  -}
  _loadPreludes :: CompileContext -> ModulePath -> IO [Expr]
  _loadPreludes ctx mod = do
    preludes <- _loadPrelude ctx mod
    if mod == []
      then return preludes
      else do
        _parents <- _loadPreludes ctx (take (length mod - 1) mod)
        return $ _parents ++ preludes

  -- Look for a single package prelude. Caches the result.
  _loadPrelude :: CompileContext -> ModulePath -> IO [Expr]
  _loadPrelude ctx mod = do
    -- look for a possible prelude module for this package
    existing <- h_lookup (context_preludes ctx) mod
    case existing of
      Just x -> return x
      Nothing -> do
        let preludePath = mod ++ ["prelude"]
        debugLog ctx $ "checking for prelude <" ++ s_unpack (showModulePath preludePath) ++ ">"
        found <- try $ findModule ctx preludePath Nothing :: IO (Either Errors FilePath)
        preludes <- case found of
          Left _ -> do return []
          Right r -> parseModuleExprs ctx preludePath Nothing
        h_insert (context_preludes ctx) mod preludes
        return preludes

  _loadModule :: CompileContext -> ModulePath -> Maybe Span -> IO Module
  _loadModule ctx mod pos = do
    debugLog ctx $ "loading module <" ++ s_unpack (showModulePath mod) ++ ">"
    exprs <- parseModuleExprs ctx mod pos
    prelude <- _loadPreludes ctx (take (length mod - 1) mod)
    m <- newMod mod (prelude ++ exprs)
    findTopLevels ctx m
    return m

  _loadImportedModule :: CompileContext -> [Error] -> (ModulePath, Span) -> IO [Error]
  _loadImportedModule ctx acc (mod, pos) = do
    result <- try $ loadModule ctx mod (Just pos)
    return $ case result of
      Left (Errs errs) -> acc ++ errs
      Right m -> acc

  parseModuleExprs :: CompileContext -> ModulePath -> Maybe Span -> IO [Expr]
  parseModuleExprs ctx mod pos = do
    path <- (findModule ctx mod pos)
    parsed <- parseFile path
    case parsed of
      ParseResult r -> return r
      Err e -> throw $ Errs [e {err_msg = "Parse error: " ++ (err_msg e)}]

  findModule :: CompileContext -> ModulePath -> Maybe Span -> IO FilePath
  findModule ctx mod pos = do
    let modPath = replaceExtension (joinPath (map s_unpack mod)) ".kit"
    debugLog ctx $ "searching for module <" ++ s_unpack (showModulePath mod) ++ ">"
    let searchPaths = [dir </> modPath | dir <- context_source_paths ctx]
    matches <- filterM doesFileExist searchPaths
    if matches == []
      then throw $ Errs $ [
        errp ImportError ("Couldn't find module <" ++ s_unpack (showModulePath mod) ++
                          ">; tried searching the following locations: \n\n" ++
                          (intercalate "\n" (map (\f -> "  - " ++ f) searchPaths)))
             pos
        ]
      else do
        debugLog ctx $ "found module <" ++ s_unpack (showModulePath mod) ++ "> at " ++ show (head matches)
        return $ head matches

  findTopLevels :: CompileContext -> Module -> IO ()
  findTopLevels ctx mod = do forM (mod_contents mod) (_checkForTopLevel ctx mod); return ()

  _checkForTopLevel :: CompileContext -> Module -> Expr -> IO ()
  _checkForTopLevel ctx m e = do
    case expr e of
      TypeDeclaration s -> h_insert (mod_types m) (structure_name s) s
      VarDeclaration v -> h_insert (mod_vars m) (lvalue_name $ var_name v) (VarBinding v)
      FunctionDeclaration f -> h_insert (mod_vars m) (function_name f) (FunctionBinding f)
      _ -> return ()
