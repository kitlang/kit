module Kit.Compiler.Context where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.FilePath
import System.Info
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Error
import Kit.HashTable
import Kit.Ir
import Kit.Log
import Kit.Parser.Span
import Kit.Str

data DuplicateGlobalNameError = DuplicateGlobalNameError ModulePath Str Span Span deriving (Eq, Show)
instance Errable DuplicateGlobalNameError where
  logError e@(DuplicateGlobalNameError mod name pos1 pos2) = do
    logErrorBasic e $ "Duplicate declaration for global name `" ++ s_unpack name ++ "` in " ++ s_unpack (showModulePath mod) ++ "; \n\nFirst declaration:"
    ePutStrLn "\nSecond declaration:"
    displayFileSnippet pos2
    ePutStrLn "\n#[extern] declarations and declarations from included C headers must have globally unique names."
  errPos (DuplicateGlobalNameError _ _ pos _) = Just pos

data CompileContext = CompileContext {
  -- state
  ctxModules :: HashTable ModulePath Module,
  ctxFailedModules :: HashTable ModulePath (),
  ctxPreludes :: HashTable ModulePath [Statement],
  ctxModuleGraph :: IORef [ModuleGraphNode],
  ctxIncludes :: IORef [FilePath],
  ctxLastTypeVar :: IORef Int,
  ctxTypeVariables :: HashTable Int TypeVarInfo,
  ctxTypedDecls :: HashTable TypePath TypedDecl,
  ctxTraitSpecializations :: HashTable TypePath (ConcreteType, Span),
  ctxImpls :: HashTable TypePath (HashTable ConcreteType (TraitImplementation TypedExpr ConcreteType)),
  ctxGlobalNames :: HashTable Str Span,
  ctxPendingGenerics :: IORef [(TypePath, [ConcreteType])],
  ctxCompleteGenerics :: HashTable (TypePath, [ConcreteType]) (),
  ctxUnresolvedTypeVars :: HashTable Int (),

  -- options
  ctxVerbose :: Int,
  ctxMainModule :: ModulePath,
  ctxIsLibrary :: Bool,
  ctxSourcePaths :: [FilePath],
  ctxCompilerPath :: Maybe FilePath,
  ctxIncludePaths :: [FilePath],
  ctxOutputDir :: FilePath,
  ctxDefines :: [(String, String)],
  ctxCompilerFlags :: [String],
  ctxLinkerFlags :: [String],
  ctxNoCompile :: Bool,
  ctxNoLink :: Bool,
  ctxDumpAst :: Bool,
  ctxNoCcache :: Bool,
  ctxRecursionLimit :: Int,
  ctxRun :: Bool
}

newCompileContext :: IO CompileContext
newCompileContext = do
  module_graph     <- newIORef []
  mods             <- h_new
  failed           <- h_new
  preludes         <- h_new
  typedDecls       <- h_new
  includes         <- newIORef []
  lastTypeVar      <- newIORef 0
  typeVars         <- h_new
  specs            <- h_new
  impls            <- h_new
  globals          <- h_new
  pendingGenerics  <- newIORef []
  completeGenerics <- h_new
  unresolved       <- h_new
  return $ CompileContext
    { ctxMainModule           = ["main"]
    , ctxIsLibrary            = False
    , ctxSourcePaths          = ["src"]
    , ctxCompilerPath         = Nothing
    , ctxIncludePaths         = ["/usr/include"]
    , ctxOutputDir            = "build"
    , ctxDefines              = []
    , ctxModules              = mods
    , ctxFailedModules        = failed
    , ctxPreludes             = preludes
    , ctxVerbose              = 0
    , ctxModuleGraph          = module_graph
    , ctxIncludes             = includes
    , ctxLastTypeVar          = lastTypeVar
    , ctxUnresolvedTypeVars   = unresolved
    , ctxTypeVariables        = typeVars
    , ctxTypedDecls           = typedDecls
    , ctxTraitSpecializations = specs
    , ctxImpls                = impls
    , ctxGlobalNames          = globals
    , ctxPendingGenerics      = pendingGenerics
    , ctxCompleteGenerics     = completeGenerics
    , ctxCompilerFlags        = []
    , ctxLinkerFlags          = []
    , ctxNoCompile            = False
    , ctxNoLink               = False
    , ctxDumpAst              = False
    , ctxNoCcache             = False
    , ctxRecursionLimit       = 256
    , ctxRun                  = True
    }

ctxSourceModules :: CompileContext -> IO [Module]
ctxSourceModules ctx = do
  mods <- (h_toList (ctxModules ctx))
  return $ filter (\m -> not (modIsCModule m)) (map snd mods)

data ModuleGraphNode
  = ModuleGraphNode ModulePath ModulePath
  deriving (Show)

getMod :: CompileContext -> ModulePath -> IO Module
getMod ctx mod = do
  m <- h_lookup (ctxModules ctx) mod
  case m of
    Just m' -> return m'
    Nothing -> throw $ KitError $ InternalError
      ("Unexpected missing module: " ++ s_unpack (showModulePath mod))
      Nothing

getCMod :: CompileContext -> FilePath -> IO Module
getCMod ctx fp = do
  let modPath = includeToModulePath fp
  m <- h_lookup (ctxModules ctx) modPath
  case m of
    Just m' -> return m'
    Nothing ->
      throwk $ InternalError ("Unexpected missing C module: " ++ fp) Nothing

makeTypeVar :: CompileContext -> Span -> IO ConcreteType
makeTypeVar ctx NoPos = throwk $ InternalError
  -- type vars from nowhere are impossible to debug, so disallow them
  ("Attempt to make type variable with no position data")
  Nothing
makeTypeVar ctx pos = do
  last <- readIORef (ctxLastTypeVar ctx)
  let next = last + 1
  writeIORef (ctxLastTypeVar ctx) next
  h_insert (ctxTypeVariables ctx)      next (newTypeVarInfo next pos)
  h_insert (ctxUnresolvedTypeVars ctx) next ()
  when (ctxVerbose ctx > 1)
    $  logMsg (Just Debug)
    $  "made type var: "
    ++ show next
    ++ " at "
    ++ show pos
  return $ TypeTypeVar next

getTypeVar :: CompileContext -> Int -> IO TypeVarInfo
getTypeVar ctx tv = do
  info <- h_get (ctxTypeVariables ctx) tv
  case typeVarValue info of
    Just (TypeTypeVar tv') -> getTypeVar ctx tv'
    _                      -> return info

resolveVar
  :: CompileContext -> [Scope Binding] -> Module -> Str -> IO (Maybe Binding)
resolveVar ctx scopes mod s = do
  local <- resolveBinding scopes s
  case local of
    Just _  -> return local
    Nothing -> do
      imports <- mapM (getMod ctx) (map fst $ modImports mod)
      resolveBinding (map modScope (mod : imports)) s

getModImports :: CompileContext -> Module -> IO [Module]
getModImports ctx mod = do
  imports   <- mapM (getMod ctx) (map fst $ modImports mod)
  includes' <- readIORef (modIncludes mod)
  includes  <- mapM (getCMod ctx) (map fst $ includes')
  return $ mod : (imports ++ includes)

makeGeneric
  :: CompileContext
  -> TypePath
  -> Span
  -> [ConcreteType]
  -> IO [(TypePath, ConcreteType)]
makeGeneric ctx tp@(modPath, name) pos existing = do
  print (tp, existing)
  defMod  <- getMod ctx modPath
  binding <- resolveLocal (modScope defMod) name
  let params = case binding of
        Just (Binding { bindingType = TypeBinding def }) -> typeParams def
        Just (Binding { bindingType = FunctionBinding def }) ->
          functionParams def
        Just (Binding { bindingType = TraitBinding def }) -> traitParams def
  if null params
    then return []
    else do
      params <-
        forM (zip params (map Just existing ++ repeat Nothing))
          $ \(param, value) -> do
        -- TODO: add param constraints here
              tv <- case value of
                Just x  -> return x
                Nothing -> makeTypeVar ctx pos
              return ((modPath ++ [name], paramName param), tv)
      let paramTypes = map snd params
      -- if the supplied type parameters are generic, this isn't a real monomorph
      unless
          (any
            (\t -> case t of
              TypeTypeParam _ -> True
              _               -> False
            )
            (map snd params)
          )
        $ modifyIORef (ctxPendingGenerics ctx) (\acc -> (tp, paramTypes) : acc)
      return params

getTypeDefinition
  :: CompileContext
  -> ModulePath
  -> Str
  -> IO (TypeDefinition TypedExpr ConcreteType)
getTypeDefinition ctx modPath typeName = do
  defMod  <- getMod ctx modPath
  binding <- resolveLocal (modScope defMod) typeName
  case binding of
    Just (Binding { bindingType = TypeBinding def }) -> return def
    _ -> throwk $ InternalError
      (  "Unexpected missing type: "
      ++ (s_unpack $ showTypePath (modPath, typeName))
      )
      Nothing

addGlobalName :: CompileContext -> Module -> Span -> Str -> IO ()
addGlobalName ctx mod pos name = do
  existing <- h_lookup (ctxGlobalNames ctx) name
  case existing of
    Just x -> throwk $ DuplicateGlobalNameError (modPath mod) name pos x
    -- If this is a C module, check for collision, but don't store to avoid
    -- collisions within the same header
    Nothing ->
      unless (modIsCModule mod) $ h_insert (ctxGlobalNames ctx) name pos

includeDir :: CompileContext -> FilePath
includeDir ctx = (ctxOutputDir ctx) </> "include"

includePath :: CompileContext -> ModulePath -> FilePath
includePath ctx mod = ((includeDir ctx) </> (moduleFilePath mod -<.> ".h"))

libDir :: CompileContext -> FilePath
libDir ctx = (ctxOutputDir ctx) </> "lib"

libPath :: CompileContext -> ModulePath -> FilePath
libPath ctx mod = (libDir ctx) </> (moduleFilePath mod -<.> ".c")

objDir :: CompileContext -> FilePath
objDir ctx = (ctxOutputDir ctx) </> "obj"

objPath :: CompileContext -> ModulePath -> FilePath
objPath ctx mod = (objDir ctx) </> (moduleFilePath mod -<.> ".o")

findStd :: IO [FilePath]
findStd = do
  -- check for overriding environment variable
  override <- lookupEnv "KIT_STD_PATH"
  case override of
    Just x  -> return [x]
    Nothing -> do
      -- check for local installation
      exePath <- getExecutablePath
      let exeDir   = takeDirectory exePath
      let localDir = exeDir </> "std"
      local <- doesDirectoryExist (localDir </> "kit")
      if local
        then return [localDir]
        else
          -- fall back to OS default
             return $ case os of
          "linux"  -> ["/usr/lib/kit"]
          "darwin" -> ["/usr/local/lib/kit"]
          _        -> []
