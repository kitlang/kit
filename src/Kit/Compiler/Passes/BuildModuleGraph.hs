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
    mainModule <- loadModule ctx (ctxMainModule ctx) Nothing
    validateMain ctx mainModule

  validateMain :: CompileContext -> Module -> IO ()
  validateMain ctx mod = do
    main <- resolveLocal (mod_vars mod) "main"
    case main of
      Just (FunctionBinding _ _ _) -> return ()
      _ -> throw $ Errs [err ValidationError $ "main module <" ++ s_unpack (showModulePath $ mod_path mod) ++ "> doesn't have a function called 'main'"]

  loadModule :: CompileContext -> ModulePath -> Maybe Span -> IO Module
  loadModule ctx mod pos = do
    existing <- h_lookup (ctxModules ctx) mod
    case existing of
      Just x -> return x
      Nothing -> do
        broken <- h_lookup (ctxFailedModules ctx) mod
        case broken of
          Just x -> do
            debugLog ctx $ "skipping known broken module <" ++ s_unpack (showModulePath mod) ++ ">"
            throw $ Errs []
          Nothing -> do
            m <- _loadModule ctx mod pos
            h_insert (ctxModules ctx) mod m
            debugLog ctx $ "module <" ++ s_unpack (showModulePath mod) ++ "> imports: " ++ (intercalate ", " (map (s_unpack . showModulePath . fst) $ mod_imports m))
            forM_ (mod_imports m) (\(mod',_) -> modifyIORef (ctxModuleGraph ctx) (\current -> ModuleGraphNode mod mod' : current))
            forM_ (mod_includes m) (\(mod',_) -> modifyIORef (ctxIncludes ctx) (\current -> mod' : current))
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
  _loadPreludes :: CompileContext -> ModulePath -> IO [Statement]
  _loadPreludes ctx mod = do
    preludes <- _loadPrelude ctx mod
    if mod == []
      then return preludes
      else do
        _parents <- _loadPreludes ctx (take (length mod - 1) mod)
        return $ _parents ++ preludes

  -- Look for a single package prelude. Caches the result.
  _loadPrelude :: CompileContext -> ModulePath -> IO [Statement]
  _loadPrelude ctx mod = do
    -- look for a possible prelude module for this package
    existing <- h_lookup (ctxPreludes ctx) mod
    case existing of
      Just x -> return x
      Nothing -> do
        let preludePath = mod ++ ["prelude"]
        debugLog ctx $ "checking for prelude <" ++ s_unpack (showModulePath preludePath) ++ ">"
        broken <- h_exists (ctxFailedModules ctx) mod
        if broken
          then return []
          else do
            found <- try $ findModule ctx preludePath Nothing :: IO (Either Errors FilePath)
            case found of
              Left _ -> do return []
              Right r -> do
                preludes <- parseModuleExprs ctx preludePath Nothing
                h_insert (ctxPreludes ctx) mod preludes
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

  parseModuleExprs :: CompileContext -> ModulePath -> Maybe Span -> IO [Statement]
  parseModuleExprs ctx mod pos = do
    path <- findModule ctx mod pos
    parsed <- parseFile path
    case parsed of
      ParseResult r -> return r
      Err e -> do
        h_insert (ctxFailedModules ctx) mod ()
        throw $ Errs [e {err_msg = "Parse error: " ++ (err_msg e)}]

  findTopLevels :: CompileContext -> Module -> IO ()
  findTopLevels ctx mod = do
    contents <- readIORef $ mod_contents mod
    forM_ contents (_checkForTopLevel ctx mod); return ()

  _checkForTopLevel :: CompileContext -> Module -> Statement -> IO ()
  _checkForTopLevel ctx m s = do
    case stmt s of
      TypeDeclaration s -> do
        debugLog ctx $ "found type " ++ s_unpack (type_name s) ++ " in module <" ++ s_unpack (showModulePath $ mod_path m) ++ ">"
        usage <- newTypeUsage s
        bindToScope (mod_types m) (type_name s) usage
      ModuleVarDeclaration v -> do
        debugLog ctx $ "found variable " ++ s_unpack (lvalue_name $ var_name v) ++ " in module <" ++ s_unpack (showModulePath $ mod_path m) ++ ">"
        varType <- _resolveMaybeType ctx (var_type v)
        bindToScope (mod_vars m) (lvalue_name $ var_name v) (VarBinding (varType))
      FunctionDeclaration f -> do
        debugLog ctx $ "found function " ++ s_unpack (function_name f) ++ " in module <" ++ s_unpack (showModulePath $ mod_path m) ++ ">"
        functionType <- _resolveMaybeType ctx (function_type f)
        args <- mapM (\arg -> do t <- _resolveMaybeType ctx (arg_type arg); return (arg_name arg, t)) (function_args f)
        bindToScope (mod_vars m) (function_name f) (FunctionBinding (functionType) args (function_varargs f))
      _ -> return ()

  _resolveMaybeType :: CompileContext -> Maybe TypeSpec -> IO TypeSpec
  _resolveMaybeType ctx t = do
    case t of
      Just t -> do return t
      Nothing -> makeTypeVar ctx
