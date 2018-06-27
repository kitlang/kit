module Kit.Compiler.Context where

  import Control.Applicative
  import Control.Monad
  import Kit.Ast
  import Kit.Compiler.Module
  import Kit.Compiler.Scope
  import Kit.HashTable
  import Kit.Str

  data CompileContext = CompileContext {
    context_main_module :: ModulePath,
    context_source_paths :: [FilePath],
    context_output_dir :: FilePath,
    context_defines :: [(String, String)],
    context_modules :: HashTable ModulePath Module,
    context_preludes :: HashTable ModulePath [Expr],
    context_verbose :: Bool
  } deriving (Show)

  compile_context = CompileContext {
    context_main_module = ["main"],
    context_source_paths = ["src"],
    context_output_dir = "build",
    context_defines = [],
    context_modules = undefined,
    context_preludes = undefined,
    context_verbose = False
  }

  findVar :: CompileContext -> [Scope] -> Module -> Str -> IO (Maybe Binding)
  findVar ctx (scope:scopes) mod s = do
    x <- h_lookup (scope_bindings scope) s
    case x of
      Just _ -> return x
      Nothing -> findVar ctx scopes mod s
  findVar ctx [] mod s = findModuleVar ctx mod s

  findModuleVar :: CompileContext -> Module -> Str -> IO (Maybe Binding)
  findModuleVar ctx mod s = do
    imports <- mapM (h_get (context_modules ctx)) (map fst (mod_imports mod))
    _searchMods ctx (mod:imports) s

  _searchMods :: CompileContext -> [Module] -> Str -> IO (Maybe Binding)
  _searchMods ctx (mod:mods) s = do
    x <- h_lookup (mod_vars mod) s
    case x of
      Just _ -> return x
      Nothing -> _searchMods ctx mods s
  _searchMods ctx [] s = do return Nothing

  _findModuleVar :: CompileContext -> Module -> Str -> IO (Maybe Binding)
  _findModuleVar ctx mod s = h_lookup (mod_vars mod) s
