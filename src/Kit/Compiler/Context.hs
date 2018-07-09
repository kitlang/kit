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
    ctxLastTypeVar :: IORef Int,
    ctxTypeVariables :: HashTable Int TypeVariableState
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
    typeVars <- h_new
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
        ctxLastTypeVar = lastTypeVar,
        ctxTypeVariables = typeVars
      }


  data ModuleGraphNode
    = ModuleGraphNode ModulePath ModulePath
    deriving (Show)

  getMod :: CompileContext -> ModulePath -> IO Module
  getMod ctx mod = do
    m <- h_lookup (ctxModules ctx) mod
    case m of
      Just m' -> return m'
      Nothing -> throw $ err InternalError $ "Unexpected missing module: " ++ s_unpack (showModulePath mod)


  getCMod :: CompileContext -> FilePath -> IO Module
  getCMod ctx f = do
    m <- h_lookup (ctxCModules ctx) f
    case m of
      Just m' -> return m'
      Nothing -> throw $ err InternalError $ "Unexpected missing C module: " ++ f


  makeTypeVar :: CompileContext -> IO ConcreteType
  makeTypeVar ctx = do
    last <- readIORef (ctxLastTypeVar ctx)
    let next = last + 1
    writeIORef (ctxLastTypeVar ctx) next
    return $ TypeVar next

  getTypeVar :: CompileContext -> TypeVar -> IO (Maybe TypeVariableState)
  getTypeVar ctx tv = h_lookup (ctxTypeVariables ctx) tv

  resolveVar :: CompileContext -> [Scope Binding] -> Module -> Str -> IO (Maybe Binding)
  resolveVar ctx scopes mod s = do
    local <- resolveBinding scopes s
    case local of
      Just _ -> return local
      Nothing -> do
        imports <- mapM (getMod ctx) (map fst $ mod_imports mod)
        resolveBinding (map mod_vars (mod : imports)) s

  resolveType :: CompileContext -> Module -> TypeSpec -> IO (Maybe ConcreteType)
  resolveType ctx mod t = do
    case t of
      ConcreteType (TypeTypedef t params) -> return $ Just (TypeTypedef t params) -- TODO: dereference immediately here
      ConcreteType ct -> return $ Just ct
      _ -> do
        -- TODO
        return Nothing

  resolveMaybeType :: CompileContext -> Module -> Maybe TypeSpec -> IO (Maybe ConcreteType)
  resolveMaybeType ctx mod t = do
    case t of
      Just t -> resolveType ctx mod t
      Nothing -> do
        var <- makeTypeVar ctx
        return $ Just var

  resolveTypeOrFail ctx mod pos t = do
    result <- resolveType ctx mod t
    case result of
      Just t -> return t
      Nothing -> throw $ Errs [errp TypingError ("Unknown type: " ++ (show t)) (Just pos)]

  resolveMaybeTypeOrFail ctx mod pos t = do
    result <- resolveMaybeType ctx mod t
    case result of
      Just t -> return t
      Nothing -> throw $ Errs [errp TypingError ("Unknown type: " ++ (show (case t of {Just x -> x}))) (Just pos)]

  resolveTypeUsage :: CompileContext -> Module -> Str -> IO (Maybe TypeUsage)
  resolveTypeUsage ctx mod s = do
    imports <- mapM (getMod ctx) (map fst $ mod_imports mod)
    resolveBinding (map mod_types (mod : imports)) s

  resolveEnum :: CompileContext -> Module -> Str -> IO (Maybe EnumConstructor)
  resolveEnum ctx mod s = do
    imports <- mapM (getMod ctx) (map fst $ mod_imports mod)
    resolveBinding (map mod_enums (mod : imports)) s

  knownType :: CompileContext -> ConcreteType -> IO ConcreteType
  knownType ctx t = do
    case t of
      TypeVar x -> do
        result <- h_lookup (ctxTypeVariables ctx) (x)
        case result of
          -- Specific known type
          Just (Right t) -> return t
          -- Constraints, but no specific type; return type var
          Just (Left _) -> return t
          -- Hasn't been unified with anything; return type var
          Nothing -> return t
      t -> return t
