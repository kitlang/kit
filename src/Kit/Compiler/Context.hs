module Kit.Compiler.Context where

  import Control.Applicative
  import Control.Exception
  import Control.Monad
  import Data.Aeson
  import Data.IORef
  import qualified Data.Text as T
  import Kit.Ast
  import Kit.Compiler.Module
  import Kit.Compiler.Scope
  import Kit.Compiler.TypeUsage
  import Kit.Error
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

  newCompileContext :: IO CompileContext
  newCompileContext = do
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

  findType :: CompileContext -> Module -> TypeSpec -> IO (Maybe ConcreteType)
  findType ctx mod t = do return Nothing -- TODO

  getMod :: CompileContext -> ModulePath -> IO Module
  getMod ctx mod = do
    m <- h_lookup (context_modules ctx) mod
    case m of
      Just m' -> return m'
      Nothing -> throw $ err InternalError $ "Unexpected missing module: " ++ s_unpack (showModulePath mod)

  resolveVar :: CompileContext -> [Scope Binding] -> Module -> Str -> IO (Maybe Binding)
  resolveVar ctx scopes mod s = do
    local <- resolveBinding scopes s
    case local of
      Just _ -> return local
      Nothing -> do
        imports <- mapM (getMod ctx) (map fst $ mod_imports mod)
        resolveBinding (map mod_vars (mod : imports)) s

  resolveType :: CompileContext -> Module -> Str -> IO (Maybe TypeUsage)
  resolveType ctx mod s = do
    imports <- mapM (getMod ctx) (map fst $ mod_imports mod)
    resolveBinding (map mod_types (mod : imports)) s
