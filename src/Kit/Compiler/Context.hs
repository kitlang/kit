module Kit.Compiler.Context where

  import Control.Applicative
  import Control.Monad
  import Data.Aeson
  import Data.IORef
  import qualified Data.Text as T
  import Kit.Ast
  import Kit.Compiler.Module
  import Kit.Compiler.Scope
  import Kit.HashTable
  import Kit.Ir
  import Kit.Str

  data CompileContext = CompileContext {
    context_main_module :: ModulePath,
    context_source_paths :: [FilePath],
    context_include_paths :: [FilePath],
    context_output_dir :: FilePath,
    context_defines :: [(String, String)],
    context_modules :: HashTable ModulePath Module,
    context_failed_modules :: HashTable ModulePath (),
    context_preludes :: HashTable ModulePath [Expr],
    context_verbose :: Bool,
    context_module_graph :: IORef [ModuleGraphNode],
    context_cmodules :: HashTable FilePath Module,
    context_includes :: IORef [FilePath]
  }
  instance Show CompileContext where
    show ctx = s_unpack $ encode $ object [
        "main" .= (s_unpack $ showModulePath $ context_main_module ctx),
        "src" .= context_source_paths ctx,
        "include" .= context_include_paths ctx,
        "out" .= context_output_dir ctx,
        "defines" .= object [T.pack key .= value | (key, value) <- context_defines ctx],
        "verbose" .= context_verbose ctx
      ]

  compile_context :: IO CompileContext
  compile_context = do
    module_graph <- newIORef []
    mods <- h_new
    failed <- h_new
    preludes <- h_new
    cmodules <- h_new
    includes <- newIORef []
    return $ CompileContext {
        context_main_module = ["main"],
        context_source_paths = ["src"],
        context_include_paths = ["/usr/include"],
        context_output_dir = "build",
        context_defines = [],
        context_modules = mods,
        context_failed_modules = failed,
        context_preludes = preludes,
        context_verbose = False,
        context_module_graph = module_graph,
        context_cmodules = cmodules,
        context_includes = includes
      }


  data ModuleGraphNode
    = ModuleGraphNode ModulePath ModulePath
    deriving (Show)

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
