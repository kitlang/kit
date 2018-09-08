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

data CompileContext = CompileContext {
  -- state
  ctxModules :: HashTable ModulePath Module,
  ctxFailedModules :: HashTable ModulePath (),
  ctxPreludes :: HashTable ModulePath [Statement],
  ctxIncludes :: IORef [FilePath],
  ctxLastTypeVar :: IORef Int,
  ctxTypeVariables :: HashTable Int TypeVarInfo,
  ctxTraitSpecializations :: HashTable TypePath (ConcreteType, Span),
  ctxImpls :: HashTable TraitConstraint (HashTable ConcreteType (TraitImplementation TypedExpr ConcreteType)),
  ctxBindings :: HashTable TypePath Binding,
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
  ctxRun :: Bool,
  ctxNameMangling :: Bool
}

newCompileContext :: IO CompileContext
newCompileContext = do
  mods             <- h_new
  failed           <- h_new
  preludes         <- h_new
  includes         <- newIORef []
  lastTypeVar      <- newIORef 0
  typeVars         <- h_newSized 1024
  specs            <- h_new
  impls            <- h_new
  -- make this big so we're less likely to have to resize later
  bindings         <- h_newSized 4096
  pendingGenerics  <- newIORef []
  completeGenerics <- h_new
  unresolved       <- h_new
  defaultIncludes  <- defaultIncludePaths
  return $ CompileContext
    { ctxMainModule           = ["main"]
    , ctxIsLibrary            = False
    , ctxSourcePaths          = ["src"]
    , ctxCompilerPath         = Nothing
    , ctxIncludePaths         = defaultIncludes
    , ctxOutputDir            = "build"
    , ctxDefines              = []
    , ctxModules              = mods
    , ctxFailedModules        = failed
    , ctxPreludes             = preludes
    , ctxVerbose              = 0
    , ctxIncludes             = includes
    , ctxLastTypeVar          = lastTypeVar
    , ctxUnresolvedTypeVars   = unresolved
    , ctxTypeVariables        = typeVars
    , ctxTraitSpecializations = specs
    , ctxImpls                = impls
    , ctxBindings             = bindings
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
    , ctxNameMangling         = True
    }

ctxSourceModules :: CompileContext -> IO [Module]
ctxSourceModules ctx = do
  mods <- (h_toList (ctxModules ctx))
  return $ filter (\m -> not (modIsCModule m)) (map snd mods)

addBinding :: CompileContext -> TypePath -> Binding -> IO ()
addBinding ctx tp b = h_insert (ctxBindings ctx) tp b

lookupBinding :: CompileContext -> TypePath -> IO (Maybe Binding)
lookupBinding ctx tp = h_lookup (ctxBindings ctx) tp

getBinding :: CompileContext -> TypePath -> IO Binding
getBinding ctx tp = do
  b <- lookupBinding ctx tp
  case b of
    Just b  -> return b
    Nothing -> throwk $ KitError $ InternalError
      ("Unexpected missing binding for " ++ s_unpack (showTypePath tp))
      Nothing

addToInterface
  :: CompileContext -> Module -> Str -> Binding -> Bool -> Bool -> IO ()
addToInterface ctx mod name b namespace allowCollisions = do
  unless allowCollisions $ do
    existing <- h_lookup (ctxBindings ctx) (modPath mod, name)
    case existing of
      Just x -> throwk $ DuplicateDeclarationError (modPath mod)
                                                   name
                                                   (bindingPos x)
                                                   (bindingPos b)
      _ -> return ()
  h_insert (ctxBindings ctx) (modPath mod, name) b

getMod :: CompileContext -> ModulePath -> IO Module
getMod ctx mod = do
  m <- h_lookup (ctxModules ctx) mod
  case m of
    Just m' -> return m'
    Nothing -> throw $ KitError $ InternalError
      ("Unexpected missing module: " ++ s_unpack (showModulePath mod))
      Nothing

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
      imports <- getModImports ctx mod
      foldM
        (\acc v -> case acc of
          Just _  -> return acc
          Nothing -> lookupBinding ctx (v, s)
        )
        Nothing
        imports

getModImports :: CompileContext -> Module -> IO [ModulePath]
getModImports ctx mod = do
  let imports = map fst $ modImports mod
  return $ (modPath mod) : (imports ++ [[]])

makeGeneric
  :: CompileContext
  -> TypePath
  -> Span
  -> [ConcreteType]
  -> IO [(TypePath, ConcreteType)]
makeGeneric ctx tp@(modPath, name) pos existing = do
  binding <- getBinding ctx tp
  let params = case binding of
        TypeBinding     def -> typeParams def
        FunctionBinding def -> functionParams def
        TraitBinding    def -> traitAllParams def
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
              return ((subPath tp $ paramName param), tv)
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
  :: CompileContext -> TypePath -> IO (TypeDefinition TypedExpr ConcreteType)
getTypeDefinition ctx tp = do
  binding <- lookupBinding ctx tp
  case binding of
    Just (TypeBinding def) -> return def
    _                      -> throwk $ InternalError
      ("Unexpected missing type: " ++ (s_unpack $ showTypePath tp))
      Nothing

getTraitDefinition
  :: CompileContext -> TypePath -> IO (TraitDefinition TypedExpr ConcreteType)
getTraitDefinition ctx tp = do
  binding <- lookupBinding ctx tp
  case binding of
    Just (TraitBinding def) -> return def
    _                       -> throwk $ InternalError
      ("Unexpected missing trait: " ++ (s_unpack $ showTypePath tp))
      Nothing

includeDir :: CompileContext -> FilePath
includeDir ctx = (ctxOutputDir ctx) </> "include"

includePath :: CompileContext -> TypePath -> FilePath
includePath ctx (n, s) =
  (includeDir ctx)
    </>  (foldr (</>) "" (map s_unpack n))
    </>  s_unpack s
    -<.> ".h"

libDir :: CompileContext -> FilePath
libDir ctx = (ctxOutputDir ctx) </> "lib"

libPath :: CompileContext -> TypePath -> FilePath
libPath ctx (n, s) =
  (libDir ctx) </> (foldr (</>) "" (map s_unpack n)) </> s_unpack s -<.> ".c"

objDir :: CompileContext -> FilePath
objDir ctx = (ctxOutputDir ctx) </> "obj"

objPath :: CompileContext -> TypePath -> FilePath
objPath ctx (n, s) =
  (objDir ctx) </> (foldr (</>) "" (map s_unpack n)) </> s_unpack s -<.> ".o"

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

defaultIncludePaths :: IO [FilePath]
defaultIncludePaths = do
  inc <- lookupEnv "INCLUDE_PATH"
  case inc of
    Just x  -> return $ splitDirs x
    Nothing -> case os of
      "linux"  -> return ["/usr/include/x86_64-linux-gnu", "/usr/include"]
      "darwin" -> return ["/usr/include"]

splitDirs f = case break (== ',') f of
  (a, ',' : b) -> a : splitDirs b
  (a, ""     ) -> [a]
