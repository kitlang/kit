module Kit.Compiler.Context where

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.Mutable
import qualified Data.Vector.Mutable as MV
import System.Directory
import System.Environment
import System.FilePath
import System.Info
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.NameMangling
import Kit.Str
import Kit.Toolchain

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
  ctxState :: CompileContextState,

  -- options
  ctxVerbose :: !Int,
  ctxMainModule :: !ModulePath,
  ctxIsLibrary :: !Bool,
  ctxSourcePaths :: ![(FilePath, ModulePath)],
  ctxBuildDir :: !FilePath,
  ctxOutputPath :: !FilePath,
  ctxNoCompile :: !Bool,
  ctxNoLink :: !Bool,
  ctxDumpAst :: !Bool,
  ctxNoCcache :: !Bool,
  ctxRecursionLimit :: !Int,
  ctxRun :: !Bool,
  ctxResultHandler :: Maybe (String -> IO ()),
  ctxNameMangling :: !Bool,
  ctxMacro :: Maybe (FunctionDefinition Expr TypeSpec, [(Int, [Expr])])
}

data CompileContextState = CompileContextState {
  ctxStateModules :: HashTable ModulePath Module,
  ctxStateIncludes :: IORef [FilePath],
  ctxStateLinkedLibs :: IORef [Str],
  ctxStateLastTypeVar :: IORef Int,
  ctxStateLastTemplateVar :: IORef Int,
  ctxStateTypeVariables :: IORef (MV.IOVector TypeVarInfo),
  ctxStateTemplateVariables :: HashTable (Int, Str) Int,
  ctxStateTraitDefaults :: HashTable TypePath (ConcreteType, Span),
  ctxStateImpls :: HashTable TraitConstraint (HashTable ConcreteType (TraitImplementation TypedExpr ConcreteType)),
  -- ctxStateGenericImpls :: HashTable
  ctxStateTypes :: HashTable TypePath SyntacticBinding,
  ctxStateBindings :: HashTable TypePath TypedBinding,
  ctxStateTypedefs :: HashTable Str ConcreteType,
  ctxStateTuples :: IORef [(ModulePath, ConcreteType)],
  ctxStatePendingGenerics :: IORef [(TypePath, [ConcreteType])],
  ctxStateCompleteGenerics :: HashTable (TypePath, [ConcreteType]) Bool,
  ctxStateUnresolvedTypeVars :: IORef [Int],
  ctxStateUnresolvedTemplateVars :: IORef [(Int, [ConcreteType], Span)],
  ctxStateConstantParamTypes :: HashTable TypePath ConcreteType,
  ctxStateMadeProgress :: IORef Bool
}

newCompileContext :: IO CompileContext
newCompileContext = do
  state <- newCtxState
  return $ CompileContext
    { ctxMainModule     = ["main"]
    , ctxIsLibrary      = False
    , ctxSourcePaths    = [("src", [])]
    , ctxBuildDir       = "build"
    , ctxOutputPath     = "main"
    , ctxVerbose        = 0
    , ctxNoCompile      = False
    , ctxNoLink         = False
    , ctxDumpAst        = False
    , ctxNoCcache       = False
    , ctxRecursionLimit = 64
    , ctxRun            = False
    , ctxResultHandler  = Nothing
    , ctxNameMangling   = True
    , ctxMacro          = Nothing
    , ctxState          = state
    }

newCtxState = do
  -- make hash tables big so we're less likely to have to resize later
  mods               <- h_newSized 256
  includes           <- newRef []
  linkedLibs         <- newRef []
  lastTypeVar        <- newRef (-1)
  lastTemplateVar    <- newRef (-1)
  defaults           <- h_newSized 256
  impls              <- h_newSized 256
  typedefs           <- h_newSized 256
  tuples             <- newRef []
  typeVars'          <- MV.new 0x10000
  typeVars           <- newRef typeVars'
  templateVars       <- h_newSized 1024
  types              <- h_newSized 256
  bindings           <- h_newSized 4096
  pendingGenerics    <- newRef []
  completeGenerics   <- h_newSized 256
  unresolved         <- newRef []
  unresolvedTemplate <- newRef []
  prog               <- newRef False
  constTypes         <- h_newSized 128
  return $ CompileContextState
    { ctxStateIncludes               = includes
    , ctxStateLinkedLibs             = linkedLibs
    , ctxStateLastTypeVar            = lastTypeVar
    , ctxStateLastTemplateVar        = lastTemplateVar
    , ctxStateUnresolvedTypeVars     = unresolved
    , ctxStateUnresolvedTemplateVars = unresolvedTemplate
    , ctxStateTypeVariables          = typeVars
    , ctxStateTemplateVariables      = templateVars
    , ctxStateTraitDefaults          = defaults
    , ctxStateImpls                  = impls
    , ctxStateTypedefs               = typedefs
    , ctxStateTuples                 = tuples
    , ctxStateTypes                  = types
    , ctxStateBindings               = bindings
    , ctxStatePendingGenerics        = pendingGenerics
    , ctxStateCompleteGenerics       = completeGenerics
    , ctxStateModules                = mods
    , ctxStateConstantParamTypes     = constTypes
    , ctxStateMadeProgress           = prog
    }

ctxModules = ctxStateModules . ctxState
ctxIncludes = ctxStateIncludes . ctxState
ctxLinkedLibs = ctxStateLinkedLibs . ctxState
ctxLastTypeVar = ctxStateLastTypeVar . ctxState
ctxLastTemplateVar = ctxStateLastTemplateVar . ctxState
ctxTypeVariables = ctxStateTypeVariables . ctxState
ctxTemplateVariables = ctxStateTemplateVariables . ctxState
ctxTraitDefaults = ctxStateTraitDefaults . ctxState
ctxImpls = ctxStateImpls . ctxState
ctxTypes = ctxStateTypes . ctxState
ctxBindings = ctxStateBindings . ctxState
ctxTypedefs = ctxStateTypedefs . ctxState
ctxTuples = ctxStateTuples . ctxState
ctxPendingGenerics = ctxStatePendingGenerics . ctxState
ctxCompleteGenerics = ctxStateCompleteGenerics . ctxState
ctxUnresolvedTypeVars = ctxStateUnresolvedTypeVars . ctxState
ctxUnresolvedTemplateVars = ctxStateUnresolvedTemplateVars . ctxState
ctxConstantParamTypes = ctxStateConstantParamTypes . ctxState
ctxMadeProgress = ctxStateMadeProgress . ctxState

ctxSourceModules :: CompileContext -> IO [Module]
ctxSourceModules ctx = do
  h_foldM (\a (_, m) -> return $ if modIsCModule m then a else (m : a))
          []
          (ctxModules ctx)

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
-- makeTypeVar ctx NoPos = throwk $ InternalError
--   -- type vars from nowhere are impossible to debug, so disallow them
--   ("Attempt to make type variable with no position data")
--   Nothing
makeTypeVar ctx pos = do
  last <- readRef (ctxLastTypeVar ctx)
  let next = last + 1
  writeRef (ctxLastTypeVar ctx) next
  vars <- readRef $ ctxTypeVariables ctx
  let len = MV.length vars
  vars <- if len <= next
    then do
      vars <- MV.grow vars (len * 2)
      writeRef (ctxTypeVariables ctx) vars
      return vars
    else return vars
  MV.write vars next (newTypeVarInfo next pos)
  modifyRef (ctxUnresolvedTypeVars ctx) (\existing -> next : existing)
  when (ctxVerbose ctx > 1)
    $  logMsg (Just Debug)
    $  "made type var: "
    ++ show next
    ++ " at "
    ++ show pos
  return $ TypeTypeVar next

updateTypeVar :: CompileContext -> Int -> TypeVarInfo -> IO ()
updateTypeVar ctx index tv = do
  vars <- readRef $ ctxTypeVariables ctx
  MV.write vars index tv

makeTemplateVar :: CompileContext -> [TypePath] -> Span -> IO ConcreteType
makeTemplateVar ctx requiredParams pos = do
  last <- readRef (ctxLastTemplateVar ctx)
  let next = last + 1
  writeRef (ctxLastTemplateVar ctx) next
  when (ctxVerbose ctx > 1)
    $  logMsg (Just Debug)
    $  "made template var: "
    ++ show next
    ++ " at "
    ++ show pos
  return $ TypeTemplateVar requiredParams next pos

getTypeVar :: CompileContext -> Int -> IO TypeVarInfo
getTypeVar ctx tv = do
  vars <- readRef $ ctxTypeVariables ctx
  info <- MV.read vars tv
  case typeVarValue info of
    Just (TypeTypeVar tv') -> getTypeVar ctx tv'
    _                      -> return info

templateVarToTypeVar
  :: CompileContext -> Int -> [ConcreteType] -> Span -> IO Int
templateVarToTypeVar ctx tv params pos = do
  let key = hashParams params
  val <- h_lookup (ctxTemplateVariables ctx) (tv, key)
  case val of
    Just i  -> return i
    Nothing -> do
      -- create a new type var for this monomorph
      (TypeTypeVar i) <- do
        makeTypeVar ctx pos
      modifyRef (ctxUnresolvedTemplateVars ctx)
                (\existing -> (tv, params, pos) : existing)
      h_insert (ctxTemplateVariables ctx) (tv, key) i
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
      foldM
        (\acc v -> case acc of
          Just _  -> return acc
          Nothing -> lookupBinding ctx (v, s)
        )
        Nothing
        (modImportPaths mod)

makeGeneric
  :: CompileContext
  -> TypePath
  -> Span
  -> [ConcreteType]
  -> IO [(TypePath, ConcreteType)]
makeGeneric ctx tp@(modPath, name) pos existing = do
  params <- do
    binding <- h_lookup (ctxBindings ctx) tp
    case binding of
      Just x -> case x of
        TypeBinding     def -> return $ typeParams def
        FunctionBinding def -> return $ functionParams def
        TraitBinding    def -> return $ traitAllParams def
      Nothing -> do
        binding <- h_get (ctxTypes ctx) tp
        let
          c = converter (\expr -> undefined)
                        (\_ t -> return $ UnresolvedType t modPath)
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
        $ do
            existing <- h_lookup (ctxCompleteGenerics ctx) (tp, paramTypes)
            case existing of
              Nothing -> do
                modifyRef (ctxPendingGenerics ctx) $ (:) (tp, paramTypes)
                h_insert (ctxCompleteGenerics ctx) (tp, paramTypes) False
              _ -> return ()

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
  TypeTuple t -> do
    t <- forM t $ makeGenericConcrete ctx pos
    return $ TypeTuple t
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

splitDirs f = case break (== ',') f of
  (a, ',' : b) -> a : splitDirs b
  (a, ""     ) -> [a]

buildDir :: CompileContext -> FilePath
buildDir ctx = ctxBuildDir ctx

findPackageContents :: CompileContext -> ModulePath -> Bool -> IO [ModulePath]
findPackageContents ctx modPath recurse = do
  x <- forM (ctxSourcePaths ctx) $ findPackageContents_ recurse modPath
  return $ foldr (++) [] x

findPackageContents_
  :: Bool -> ModulePath -> (FilePath, ModulePath) -> IO [ModulePath]
findPackageContents_ recurse m (dir, []) = do
  let dirPath = dir </> (moduleFilePath m -<.> "")
  exists <- doesDirectoryExist dirPath
  if not exists
    then return []
    else do
      files <- listDirectory dirPath
      if recurse
        then do
          children <- forM files $ \file -> do
            isDir <- doesDirectoryExist $ dirPath </> file
            if isDir
              then findPackageContents_ True (m ++ [s_pack file]) (dir, [])
              else if isModule file
                then return [m ++ [s_pack $ file -<.> ""]]
                else return []
          return $ foldr (++) [] children
        else
          return
            $ [ m ++ [s_pack $ file -<.> ""] | file <- files, isModule file ]
findPackageContents_ recurse (m : n) (dir, (h : t)) = if m == h
  then do
    contents <- findPackageContents_ recurse n (dir, t)
    return $ [ (h : c) | c <- contents ]
  else return []
findPackageContents_ _ _ _ = return []

isModule file = (takeExtension file == ".kit") && (file /= "prelude.kit")

interfaceTypeConverter
  :: CompileContext
  -> Module
  -> Span
  -> [TypePath]
  -> TypeSpec
  -> IO ConcreteType
interfaceTypeConverter ctx mod pos typeParams (InferredType _) =
  makeTypeVar ctx pos
interfaceTypeConverter ctx mod pos typeParams (ConstantTypeSpec v _) =
  return $ ConstantType v
interfaceTypeConverter ctx mod pos (h : t) x@(TypeSpec tp [] _) =
  if h == tp || ((null $ fst tp) && tpName tp == tpName h)
    then return $ TypeTypeParam h
    else interfaceTypeConverter ctx mod pos t x
interfaceTypeConverter ctx mod pos typeParams x =
  return $ UnresolvedType x $ modPath mod

addStmtToModuleInterface
  :: CompileContext -> Module -> SyntacticStatement -> IO [SyntacticStatement]
addStmtToModuleInterface ctx mod s = do
  -- the expressions from these conversions shouldn't be used;
  -- we'll use the actual typed versions generated later
  let interfaceParamConverter tp params = converter
        (\e -> do
          tv <- if null params
            then makeTypeVar ctx (ePos e)
            else makeTemplateVar ctx params (ePos e)
          return $ makeExprTyped (This) tv (ePos e)
        )
        (\pos t -> interfaceTypeConverter ctx mod pos params t)
  let interfaceConverter = interfaceParamConverter ([], "") []
  decls <- case stmt s of
    Typedef ([], name) t -> do
      return [s { stmt = Typedef (modPath mod, name) t }]

    TypeDeclaration d -> do
      let extern = hasMeta "extern" (typeMeta d)
      let name   = tpName $ typeName d
      let tp     = (if extern then [] else modPath mod, name)
      d <- return $ d { typeName = tp }
      h_insert (ctxTypes ctx) tp $ TypeBinding d
      return [s { stmt = TypeDeclaration d }]

    TraitDeclaration d -> do
      let name = tpName $ traitName d
      let tp   = (modPath mod, name)
      d <- return $ d { traitName = tp }
      h_insert (ctxTypes ctx) tp $ TraitBinding d
      return [s { stmt = TraitDeclaration d }]

    VarDeclaration d -> do
      let extern = hasMeta "extern" (varMeta d)
      let name   = tpName $ varName d
      let tp     = (if extern then [] else modPath mod, name)
      converted <- convertVarDefinition interfaceConverter (d { varName = tp })
      when extern $ recordGlobalName name
      d <- return $ d { varName = tp }
      return [s { stmt = VarDeclaration d }]

    FunctionDeclaration d@(FunctionDefinition { functionVararg = vararg }) ->
      let isMain =
            functionName d == ([], "main") && (ctxMainModule ctx == modPath mod)
      in  case (ctxMacro ctx, isMain) of
            (Just _, True) -> -- filter out main for macros
              return []
            _ -> do
              let extern = (hasMeta "extern" (functionMeta d)) || isMain
              let name   = tpName $ functionName d
              let tp     = (if extern then [] else modPath mod, name)
              when extern $ recordGlobalName name
              d <- return $ d { functionName = tp }
              return [s { stmt = FunctionDeclaration d }]

    RuleSetDeclaration r -> do
      let name = tpName $ ruleSetName r
      let tp   = (modPath mod, name)
      r <- return $ r { ruleSetName = tp }
      h_insert (ctxTypes ctx) tp $ RuleSetBinding r
      return [s { stmt = RuleSetDeclaration r }]

    TraitDefault a b -> do
      modifyRef (modDefaults mod) (\l -> ((a, b), stmtPos s) : l)
      return []

    Implement i -> do
      let
        impl = i
          { implName = ( modPath mod
                       , s_hash
                         ( s_concat
                         $ map (s_pack . show) [implTrait i, implFor i]
                         )
                       )
          }
      return [s { stmt = Implement impl }]

    ModuleUsing _ -> do
      return [s]
    ExtendDefinition _ _ -> do
      return [s]
    MacroDeclaration _ -> do
      return [s]
    MacroCall _ _ -> do
      return [s]

    _ -> return []

  return decls
 where
  recordGlobalName name = do
    existing <- lookupBinding ctx ([], name)
    case existing of
      Just b -> throwk $ DuplicateGlobalNameError (modPath mod)
                                                  name
                                                  (bindingPos b)
                                                  (stmtPos s)
      _ -> return ()

markProgress :: CompileContext -> Str -> IO ()
markProgress ctx reason = do
  when (ctxVerbose ctx > 0) $ logMsg (Just Debug) $ s_unpack reason
  writeRef (ctxMadeProgress ctx) True
