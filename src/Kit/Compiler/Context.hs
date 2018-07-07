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
    ctxMainModule :: ModulePath,
    ctxSourcePaths :: [FilePath],
    ctxIncludePaths :: [FilePath],
    ctxOutputDir :: FilePath,
    ctxDefines :: [(String, String)],
    ctxModules :: HashTable ModulePath Module,
    ctxFailedModules :: HashTable ModulePath (),
    ctxPreludes :: HashTable ModulePath [Statement],
    ctxVerbose :: Bool,
    ctxModuleGraph :: IORef [ModuleGraphNode],
    ctxCModules :: HashTable FilePath Module,
    ctxIncludes :: IORef [FilePath],
    ctxLastTypeVar :: IORef Int
  }
  instance Show CompileContext where
    show ctx = s_unpack $ encode $ object [
        "main" .= (s_unpack $ showModulePath $ ctxMainModule ctx),
        "src" .= ctxSourcePaths ctx,
        "include" .= ctxIncludePaths ctx,
        "out" .= ctxOutputDir ctx,
        "defines" .= object [T.pack key .= value | (key, value) <- ctxDefines ctx],
        "verbose" .= ctxVerbose ctx
      ]

  newCompileContext :: IO CompileContext
  newCompileContext = do
    module_graph <- newIORef []
    mods <- h_new
    failed <- h_new
    preludes <- h_new
    cmodules <- h_new
    includes <- newIORef []
    lastTypeVar <- newIORef 0
    return $ CompileContext {
        ctxMainModule = ["main"],
        ctxSourcePaths = ["src"],
        ctxIncludePaths = ["/usr/include"],
        ctxOutputDir = "build",
        ctxDefines = [],
        ctxModules = mods,
        ctxFailedModules = failed,
        ctxPreludes = preludes,
        ctxVerbose = False,
        ctxModuleGraph = module_graph,
        ctxCModules = cmodules,
        ctxIncludes = includes,
        ctxLastTypeVar = lastTypeVar
      }


  data ModuleGraphNode
    = ModuleGraphNode ModulePath ModulePath
    deriving (Show)

  findType :: CompileContext -> Module -> TypeSpec -> IO (Maybe ConcreteType)
  findType ctx mod t = do return Nothing -- TODO

  getMod :: CompileContext -> ModulePath -> IO Module
  getMod ctx mod = do
    m <- h_lookup (ctxModules ctx) mod
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

  makeTypeVar :: CompileContext -> IO (TypeSpec)
  makeTypeVar ctx = do
    last <- readIORef (ctxLastTypeVar ctx)
    let next = last + 1
    writeIORef (ctxLastTypeVar ctx) next
    return $ TypeVar next
