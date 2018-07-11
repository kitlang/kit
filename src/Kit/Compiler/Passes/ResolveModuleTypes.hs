module Kit.Compiler.Passes.ResolveModuleTypes where

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
  import Kit.Compiler.TypeContext
  import Kit.Compiler.TypeUsage
  import Kit.Compiler.Utils
  import Kit.Error
  import Kit.HashTable
  import Kit.Log
  import Kit.Parser
  import Kit.Str

  resolveModuleTypes :: CompileContext -> IO ()
  resolveModuleTypes ctx = do
    mods <- h_toList $ ctxModules ctx
    forM_ (map snd mods) (findTopLevels ctx)
    validateMain ctx
    return ()

  validateMain :: CompileContext -> IO ()
  validateMain ctx = do
    mod <- getMod ctx (ctxMainModule ctx)
    main <- resolveLocal (mod_vars mod) "main"
    case main of
      Just (FunctionBinding _ _ _) -> return ()
      _ -> throw $ Errs [err ValidationError $ (show mod) ++ " doesn't have a function called 'main'; main module requires a main function"]

  findTopLevels :: CompileContext -> Module -> IO ()
  findTopLevels ctx mod = do
    contents <- readIORef $ mod_contents mod
    forM_ contents (_checkForTopLevel ctx mod); return ()

  {-
    Parse module toplevel statements, which can be type, function, or variable
    declarations, and try to resolve their types recursively. Fail here if any
    unknown types are referenced.
  -}
  _checkForTopLevel :: CompileContext -> Module -> Statement -> IO ()
  _checkForTopLevel ctx m s = do
    tctx <- newTypeContext []
    case stmt s of
      TypeDeclaration t@(TypeDefinition {type_type = Typedef {typedef_definition = typedefType}}) -> do
        debugLog ctx $ "found typedef " ++ s_unpack (type_name t) ++ " in " ++ (show m)
        resolved <- resolveType ctx tctx m typedefType
        bindToScope (mod_types m) (type_name t) resolved
      TypeDeclaration t -> do
        debugLog ctx $ "found type " ++ s_unpack (type_name t) ++ " in " ++ (show m)
        usage <- newTypeUsage t
        bindToScope (mod_type_definitions m) (type_name t) usage
        case t of
          TypeDefinition {type_name = type_name, type_type = Enum {enum_variants = variants}} -> do
            forM_ (variants) (\variant -> do
              args <- mapM (\arg -> do t <- resolveMaybeType ctx tctx m (arg_type arg); return (arg_name arg, t)) (variant_args variant)
              let constructor = ((mod_path m, type_name), args)
              bindToScope (mod_enums m) (variant_name variant) constructor
              return ())
          _ -> do return ()
      ModuleVarDeclaration v -> do
        debugLog ctx $ "found variable " ++ s_unpack (lvalue_name $ var_name v) ++ " in " ++ (show m)
        varType <- resolveMaybeType ctx tctx m (var_type v)
        bindToScope (mod_vars m) (lvalue_name $ var_name v) (VarBinding (varType))
      FunctionDeclaration f -> do
        debugLog ctx $ "found function " ++ s_unpack (function_name f) ++ " in " ++ (show m)
        functionType <- resolveMaybeType ctx tctx m (function_type f)
        args <- mapM (\arg -> do t <- resolveMaybeType ctx tctx m (arg_type arg); return (arg_name arg, t)) (function_args f)
        bindToScope (mod_vars m) (function_name f) (FunctionBinding (functionType) args (function_varargs f))
        bindToScope (mod_functions m) (function_name f) f
      _ -> return ()
