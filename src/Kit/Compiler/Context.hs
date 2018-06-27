module Kit.Compiler.Context where

  import Control.Applicative
  import Control.Monad
  import Kit.Ast
  import Kit.Compiler.Module
  import Kit.Compiler.Scope
  import Kit.Hash
  import Kit.Str

  data CompileContext = CompileContext {
    context_main_module :: ModulePath,
    context_source_paths :: [FilePath],
    context_output_dir :: FilePath,
    context_defines :: [(String, String)],
    context_modules :: HashTable ModulePath Module
  } deriving (Show)

  compile_context = CompileContext {
    context_main_module = ["main"],
    context_source_paths = ["src"],
    context_output_dir = "build",
    context_defines = [],
    context_modules = undefined
  }

  findVar :: CompileContext -> [Scope] -> Module -> Str -> IO (Maybe Binding)
  findVar ctx (scope:scopes) mod s = do
    x <- h_lookup (scope_bindings scope) s
    y <- findVar ctx scopes mod s
    return $ x <|> y
  findVar ctx [] mod s = findModuleVar ctx mod s

  findModuleVar :: CompileContext -> Module -> Str -> IO (Maybe Binding)
  findModuleVar ctx mod s = do
    imports <- mapM (h_get (context_modules ctx)) (map fst (mod_imports mod))
    let mods = mod : imports
    foldM (\acc m -> do x <- _findModuleVar ctx m s; return $ acc <|> x) Nothing mods

  _findModuleVar :: CompileContext -> Module -> Str -> IO (Maybe Binding)
  _findModuleVar ctx mod s = h_lookup (mod_vars mod) s


    {-existing <- h_lookup (mod_vars mod) s
    case existing of
      Just x -> return existing
      Nothing -> do
        imports <- fmap (\d -> h_get (context_modules ctx) imp) (mod_imports mod)
        foldM (\acc m -> do y <- findModuleVar ctx m s; return $ acc <|> y) Nothing imports-}
