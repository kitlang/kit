module Kit.Compiler.Context where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.Info
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypedExpr
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.NameMangling
import Kit.Str

data CompileContext = CompileContext {
  -- state
  ctxModules :: HashTable ModulePath Module,
  ctxFailedModules :: HashTable ModulePath (),
  ctxPreludes :: HashTable ModulePath [Statement],
  ctxIncludes :: IORef [FilePath],
  ctxLinkedLibs :: IORef [Str],
  ctxLastTypeVar :: IORef Int,
  ctxLastTemplateVar :: IORef Int,
  ctxTypeVariables :: HashTable Int TypeVarInfo,
  ctxTemplateVariables :: HashTable Int (HashTable Str Int),
  ctxTraitSpecializations :: HashTable TypePath (ConcreteType, Span),
  ctxImpls :: HashTable TraitConstraint (HashTable ConcreteType (TraitImplementation TypedExpr ConcreteType)),
  -- ctxGenericImpls :: HashTable
  ctxTypes :: HashTable TypePath SyntacticBinding,
  ctxBindings :: HashTable TypePath TypedBinding,
  ctxTypedefs :: HashTable Str ConcreteType,
  ctxPendingGenerics :: IORef [(TypePath, [ConcreteType])],
  ctxCompleteGenerics :: HashTable TypePath (HashTable [ConcreteType] ()),
  ctxUnresolvedTypeVars :: IORef [Int],
  ctxUnresolvedTemplateVars :: IORef [(Int, [ConcreteType], Span)],

  -- options
  ctxVerbose :: Int,
  ctxMainModule :: ModulePath,
  ctxIsLibrary :: Bool,
  ctxSourcePaths :: [FilePath],
  ctxCompilerPath :: Maybe FilePath,
  ctxIncludePaths :: [FilePath],
  ctxBuildDir :: FilePath,
  ctxOutputPath :: FilePath,
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
  mods               <- h_new
  failed             <- h_new
  preludes           <- h_new
  includes           <- newIORef []
  linkedLibs         <- newIORef []
  lastTypeVar        <- newIORef 0
  lastTemplateVar    <- newIORef 0
  specs              <- h_new
  impls              <- h_new
  typedefs           <- h_new
  -- make these big so we're less likely to have to resize later
  typeVars           <- h_newSized 4096
  templateVars       <- h_newSized 2048
  types              <- h_newSized 256
  bindings           <- h_newSized 4096
  pendingGenerics    <- newIORef []
  completeGenerics   <- h_new
  unresolved         <- newIORef []
  unresolvedTemplate <- newIORef []
  defaultIncludes    <- defaultIncludePaths
  return $ CompileContext
    { ctxMainModule             = ["main"]
    , ctxIsLibrary              = False
    , ctxSourcePaths            = ["src"]
    , ctxCompilerPath           = Nothing
    , ctxIncludePaths           = defaultIncludes
    , ctxBuildDir               = "build"
    , ctxOutputPath             = "main"
    , ctxDefines                = []
    , ctxModules                = mods
    , ctxFailedModules          = failed
    , ctxPreludes               = preludes
    , ctxVerbose                = 0
    , ctxIncludes               = includes
    , ctxLinkedLibs             = linkedLibs
    , ctxLastTypeVar            = lastTypeVar
    , ctxLastTemplateVar        = lastTemplateVar
    , ctxUnresolvedTypeVars     = unresolved
    , ctxUnresolvedTemplateVars = unresolvedTemplate
    , ctxTypeVariables          = typeVars
    , ctxTemplateVariables      = templateVars
    , ctxTraitSpecializations   = specs
    , ctxImpls                  = impls
    , ctxTypedefs               = typedefs
    , ctxTypes                  = types
    , ctxBindings               = bindings
    , ctxPendingGenerics        = pendingGenerics
    , ctxCompleteGenerics       = completeGenerics
    , ctxCompilerFlags          = []
    , ctxLinkerFlags            = []
    , ctxNoCompile              = False
    , ctxNoLink                 = False
    , ctxDumpAst                = False
    , ctxNoCcache               = False
    , ctxRecursionLimit         = 64
    , ctxRun                    = False
    , ctxNameMangling           = True
    }

ctxSourceModules :: CompileContext -> IO [Module]
ctxSourceModules ctx = do
  mods <- (h_toList (ctxModules ctx))
  return $ filter (\m -> not (modIsCModule m)) (map snd mods)

addBinding :: CompileContext -> TypePath -> TypedBinding -> IO ()
addBinding ctx tp b = h_insert (ctxBindings ctx) tp b

addTypeBinding :: CompileContext -> TypePath -> SyntacticBinding -> IO ()
addTypeBinding ctx tp b = h_insert (ctxTypes ctx) tp b

lookupBinding :: CompileContext -> TypePath -> IO (Maybe TypedBinding)
lookupBinding ctx tp = h_lookup (ctxBindings ctx) tp

getBinding :: CompileContext -> TypePath -> IO TypedBinding
getBinding ctx tp = do
  b <- lookupBinding ctx tp
  case b of
    Just b  -> return b
    Nothing -> throwk $ KitError $ InternalError
      ("Unexpected missing binding for " ++ s_unpack (showTypePath tp))
      Nothing

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
  h_insert (ctxTypeVariables ctx) next (newTypeVarInfo next pos)
  modifyIORef (ctxUnresolvedTypeVars ctx) (\existing -> next : existing)
  when (ctxVerbose ctx > 1)
    $  logMsg (Just Debug)
    $  "made type var: "
    ++ show next
    ++ " at "
    ++ show pos
  return $ TypeTypeVar next

makeTemplateVar :: CompileContext -> [TypePath] -> Span -> IO ConcreteType
makeTemplateVar ctx requiredParams pos = do
  last <- readIORef (ctxLastTemplateVar ctx)
  let next = last + 1
  writeIORef (ctxLastTemplateVar ctx) next
  vals <- h_new
  h_insert (ctxTemplateVariables ctx) next vals
  when (ctxVerbose ctx > 1)
    $  logMsg (Just Debug)
    $  "made template var: "
    ++ show next
    ++ " at "
    ++ show pos
  return $ TypeTemplateVar requiredParams next pos

getTypeVar :: CompileContext -> Int -> IO TypeVarInfo
getTypeVar ctx tv = do
  info <- h_get (ctxTypeVariables ctx) tv
  case typeVarValue info of
    Just (TypeTypeVar tv') -> getTypeVar ctx tv'
    _                      -> return info

templateVarToTypeVar
  :: CompileContext -> Int -> [ConcreteType] -> Span -> IO Int
templateVarToTypeVar ctx tv params pos = do
  let key = hashParams params
  vals <- h_get (ctxTemplateVariables ctx) tv
  val  <- h_lookup vals key
  case val of
    Just i  -> return i
    Nothing -> do
      -- create a new type var for this monomorph
      (TypeTypeVar i) <- makeTypeVar ctx pos
      modifyIORef (ctxUnresolvedTemplateVars ctx)
                  (\existing -> (tv, params, pos) : existing)
      h_insert vals key i
      return i

resolveVar
  :: CompileContext
  -> [Scope TypedBinding]
  -> Module
  -> Str
  -> IO (Maybe TypedBinding)
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
  when (ctxVerbose ctx > 2)
    $  logMsg (Just Debug)
    $  "make generic: "
    ++ s_unpack (showTypePath tp)
    ++ show existing
  params <- do
    binding <- h_lookup (ctxBindings ctx) tp
    case binding of
      Just x -> case x of
        TypeBinding     def -> return $ typeParams def
        FunctionBinding def -> return $ functionParams def
        TraitBinding    def -> return $ traitAllParams def
      Nothing -> do
        binding <- h_get (ctxTypes ctx) tp
        let c = converter (\expr -> undefined)
                          (\_ (Just t) -> return $ UnresolvedType t modPath)
        let params = case binding of
              TypeBinding     def -> typeParams def
              FunctionBinding def -> functionParams def
              TraitBinding    def -> traitAllParams def
        forMWithErrors params $ convertTypeParam c

  if null params
    then return []
    else do
      params <-
        forM (zip params (map Just existing ++ repeat Nothing))
          $ \(param, value) -> do
        -- TODO: add param constraints here
              tv <- case value of
                Just x  -> makeGenericConcrete ctx pos x
                Nothing -> do
                  case typeParamDefault param of
                    Just x -> return x
                    _      -> makeTypeVar ctx pos
              return ((subPath tp $ paramName param), tv)
      let paramTypes = map snd params
      -- if the supplied type parameters are generic, this isn't a real monomorph
      unless
          (or $ foldr (++) [] $ map
            (mapType_
              (\t -> case t of
                TypeTypeParam _ -> True
                _               -> False
              )
            )
            (map snd params)
          )
        $ modifyIORef (ctxPendingGenerics ctx) (\acc -> (tp, paramTypes) : acc)
      return params

makeGenericConcrete :: CompileContext -> Span -> ConcreteType -> IO ConcreteType
makeGenericConcrete ctx pos t = case t of
  TypePtr t -> do
    t <- makeGenericConcrete ctx pos t
    return $ TypePtr t
  TypeInstance tp p -> do
    params <- makeGeneric ctx tp pos p
    return $ TypeInstance tp $ map snd params
  TypeBox tp p -> do
    params <- makeGeneric ctx tp pos p
    return $ TypeBox tp $ map snd params
  TypeTraitConstraint (tp, p) -> do
    params <- makeGeneric ctx tp pos p
    return $ TypeTraitConstraint (tp, map snd params)
  _ -> return t

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
includeDir ctx = (buildDir ctx) </> "include"

includePath :: CompileContext -> FilePath
includePath ctx = (includeDir ctx) </> (ctxOutputPath ctx) -<.> ".h"

libDir :: CompileContext -> FilePath
libDir ctx = (buildDir ctx) </> "lib"

libPath :: CompileContext -> TypePath -> FilePath
libPath ctx (n, s) =
  (libDir ctx)
    </>  (foldr (</>) "" (map s_unpack n))
    </>  ("kit_" ++ s_unpack s)
    -<.> ".c"

objDir :: CompileContext -> FilePath
objDir ctx = (buildDir ctx) </> "obj"

objPath :: CompileContext -> TypePath -> FilePath
objPath ctx (n, s) =
  (objDir ctx)
    </>  (foldr (</>) "" (map s_unpack n))
    </>  ("kit_" ++ s_unpack s)
    -<.> ".o"

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
      "darwin" -> return ["/usr/include", "/usr/local/include"]
      _        -> return []

splitDirs f = case break (== ',') f of
  (a, ',' : b) -> a : splitDirs b
  (a, ""     ) -> [a]


buildDir :: CompileContext -> FilePath
buildDir ctx = ctxBuildDir ctx

getCompilerFlags :: CompileContext -> IO [String]
getCompilerFlags ctx = do
  let ctxFlags = ctxCompilerFlags ctx
  envFlags <- lookupEnv "COMPILER_FLAGS"
  return $ case envFlags of
    -- FIXME
    Just s  -> ctxFlags ++ words s
    Nothing -> ctxFlags

getLinkerFlags :: CompileContext -> IO [String]
getLinkerFlags ctx = do
  linkedLibs <- readIORef $ ctxLinkedLibs ctx
  let ctxFlags =
        [ "-l" ++ s_unpack lib | lib <- nub linkedLibs ] ++ ctxLinkerFlags ctx
  envFlags <- lookupEnv "LINKER_FLAGS"
  return $ case envFlags of
    -- FIXME
    Just s  -> ctxFlags ++ words s
    Nothing -> ctxFlags

findCompiler :: CompileContext -> IO FilePath
findCompiler ctx = do
  -- use the --cc flag if provided
  case ctxCompilerPath ctx of
    Just cc -> return cc
    _       -> do
      -- use a CC environment variable if it exists
      ccEnv <- lookupEnv "CC"
      case ccEnv of
        Just cc -> return cc
        Nothing -> do
          -- look for a compiler next to kitc
          let exes = ["cc", "gcc", "clang"]
          exePath <- getExecutablePath
          let exeDir = takeDirectory exePath
          exePaths <- mapM (findFile [exeDir]) exes
          case msum exePaths of
            Just cc -> return cc
            Nothing -> do
              -- search the user's executable paths
              exePaths <- mapM findExecutable exes
              case msum exePaths of
                Just cc -> return cc
                Nothing -> throwk $ BasicError
                  ("Couldn't find a C compiler from your executable paths; tried looking for:\n\n"
                  ++ intercalate "\n" ([ "  - " ++ exe | exe <- exes ])
                  ++ "\n\nYou can set the compiler path explicitly using the CC environment variable"
                  )
                  Nothing

findCcache :: CompileContext -> IO (Maybe FilePath)
findCcache ctx =
  if ctxNoCcache ctx then return Nothing else findExecutable "ccache"

defaultCompileArgs :: CompileContext -> FilePath -> [String]
defaultCompileArgs ctx cc =
  [ "-D_GNU_SOURCE"
    , "-D_BSD_SOURCE"
    , "-D_DEFAULT_SOURCE"
    , "-std=c99"
    , "-pedantic"
    , "-O3"
    , "-Os"
    ]
    ++ (if ctxIsLibrary ctx then ["-fPIC"] else [])
    ++ osSpecificDefaultCompileArgs os
    ++ ccSpecificDefaultCompileArgs cc

osSpecificDefaultCompileArgs "darwin" =
  [ "-U__BLOCKS__"
  , "-Wno-expansion-to-defined"
  , "-Wno-gnu-zero-variadic-macro-arguments"
  ]
osSpecificDefaultCompileArgs _ = []

ccSpecificDefaultCompileArgs "gcc" = ["-Wno-missing-braces"]
ccSpecificDefaultCompileArgs _     = []
