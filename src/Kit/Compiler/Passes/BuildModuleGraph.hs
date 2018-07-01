module Kit.Compiler.Passes.BuildModuleGraph where

  import Control.Exception
  import Control.Monad
  import Data.IORef
  import Data.List
  import System.Directory
  import System.FilePath
  import Kit.Ast
  import Kit.Compiler.Context
  import Kit.Compiler.Module
  import Kit.Compiler.Scope
  import Kit.Compiler.TypeUsage
  import Kit.Compiler.Utils
  import Kit.Error
  import Kit.HashTable
  import Kit.Log
  import Kit.Parser
  import Kit.Str

  buildModuleGraph :: CompileContext -> IO ()
  buildModuleGraph ctx = do
    mainModule <- loadModule ctx (context_main_module ctx) Nothing
    validateMain ctx mainModule

  validateMain :: CompileContext -> Module -> IO ()
  validateMain ctx mod = do
    main <- h_lookup (mod_vars mod) "main"
    case main of
      Just (FunctionBinding f@FunctionDefinition{function_name = "main"}) -> return ()
      _ -> throw $ Errs [err ValidationError $ "main module <" ++ s_unpack (showModulePath $ mod_path mod) ++ "> doesn't have a function called 'main'"]

  loadModule :: CompileContext -> ModulePath -> Maybe Span -> IO Module
  loadModule ctx mod pos = do
    existing <- h_lookup (context_modules ctx) mod
    case existing of
      Just x -> return x
      Nothing -> do
        broken <- h_lookup (context_failed_modules ctx) mod
        case broken of
          Just x -> do
            debugLog ctx $ "skipping known broken module <" ++ s_unpack (showModulePath mod) ++ ">"
            throw $ Errs []
          Nothing -> do
            m <- _loadModule ctx mod pos
            h_insert (context_modules ctx) mod m
            debugLog ctx $ "module <" ++ s_unpack (showModulePath mod) ++ "> imports: " ++ (intercalate ", " (map (s_unpack . showModulePath . fst) $ mod_imports m))
            forM (mod_imports m) (\(mod',_) -> modifyIORef (context_module_graph ctx) (\current -> ModuleGraphNode mod mod' : current))
            forM (mod_includes m) (\(mod',_) -> modifyIORef (context_includes ctx) (\current -> mod' : current))
            errs <- foldM (_loadImportedModule ctx) [] (mod_imports m)
            if errs == []
              then return m
              else throw $ Errs $ nub errs

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
        broken <- h_exists (context_failed_modules ctx) mod
        if broken
          then return []
          else do
            found <- try $ findModule ctx preludePath Nothing :: IO (Either Errors FilePath)
            case found of
              Left _ -> do return []
              Right r -> do
                preludes <- parseModuleExprs ctx preludePath Nothing
                h_insert (context_preludes ctx) mod preludes
                return preludes

  _loadModule :: CompileContext -> ModulePath -> Maybe Span -> IO Module
  _loadModule ctx mod pos = do
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
    path <- findModule ctx mod pos
    parsed <- parseFile path
    case parsed of
      ParseResult r -> return r
      Err e -> do
        h_insert (context_failed_modules ctx) mod ()
        throw $ Errs [e {err_msg = "Parse error: " ++ (err_msg e)}]

  findTopLevels :: CompileContext -> Module -> IO ()
  findTopLevels ctx mod = do
    contents <- readIORef $ mod_contents mod
    forM contents (_checkForTopLevel ctx mod); return ()

  _checkForTopLevel :: CompileContext -> Module -> Expr -> IO ()
  _checkForTopLevel ctx m e = do
    case expr e of
      TypeDeclaration s -> do
        debugLog ctx $ "found type " ++ s_unpack (type_name s) ++ " in module <" ++ s_unpack (showModulePath $ mod_path m) ++ ">"
        usage <- newTypeUsage s
        h_insert (mod_types m) (type_name s) usage
      VarDeclaration v -> do
        debugLog ctx $ "found variable " ++ s_unpack (lvalue_name $ var_name v) ++ " in module <" ++ s_unpack (showModulePath $ mod_path m) ++ ">"
        h_insert (mod_vars m) (lvalue_name $ var_name v) (VarBinding v)
      FunctionDeclaration f -> do
        debugLog ctx $ "found function " ++ s_unpack (function_name f) ++ " in module <" ++ s_unpack (showModulePath $ mod_path m) ++ ">"
        h_insert (mod_vars m) (function_name f) (FunctionBinding f)
      _ -> return ()
