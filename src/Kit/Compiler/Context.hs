module Kit.Compiler.Context where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.IORef
import qualified Data.Text as T
import System.Directory
import System.FilePath
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypedDecl
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
  ctxMainModule :: ModulePath,
  ctxIsLibrary :: Bool,
  ctxSourcePaths :: [FilePath],
  ctxIncludePaths :: [FilePath],
  ctxOutputDir :: FilePath,
  ctxDefines :: [(String, String)],
  ctxModules :: HashTable ModulePath Module,
  ctxFailedModules :: HashTable ModulePath (),
  ctxPreludes :: HashTable ModulePath [Statement],
  ctxVerbose :: Bool,
  ctxModuleGraph :: IORef [ModuleGraphNode],
  ctxIncludes :: IORef [FilePath],
  ctxLastTypeVar :: IORef Int,
  ctxTypeVariables :: HashTable Int TypeVarInfo,
  ctxTypedDecls :: HashTable TypePath TypedDecl,
  ctxTraitSpecializations :: HashTable TypePath (TypeSpec, Span),
  ctxImpls :: HashTable TypePath (HashTable ConcreteType (TraitImplementation Expr (Maybe TypeSpec))),
  ctxGlobalNames :: HashTable Str Span,
  ctxNoCompile :: Bool,
  ctxNoLink :: Bool,
  ctxDumpAst :: Bool,
  ctxRecursionLimit :: Int
}

instance Show CompileContext where
  show ctx = s_unpack $ encode $ object [
      "main" .= (s_unpack $ showModulePath $ ctxMainModule ctx),
      "lib" .= ctxIsLibrary ctx,
      "src" .= ctxSourcePaths ctx,
      "include" .= ctxIncludePaths ctx,
      "out" .= ctxOutputDir ctx,
      "defines" .= object [T.pack key .= value | (key, value) <- ctxDefines ctx],
      "verbose" .= ctxVerbose ctx,
      "no-compile" .= ctxNoCompile ctx,
      "no-link" .= ctxNoLink ctx
    ]

newCompileContext :: IO CompileContext
newCompileContext = do
  module_graph <- newIORef []
  mods         <- h_new
  failed       <- h_new
  preludes     <- h_new
  typedDecls   <- h_new
  includes     <- newIORef []
  lastTypeVar  <- newIORef 0
  typeVars     <- h_new
  specs        <- h_new
  impls        <- h_new
  globals      <- h_new
  return $ CompileContext
    { ctxMainModule           = ["main"]
    , ctxIsLibrary            = False
    , ctxSourcePaths          = ["src"]
    , ctxIncludePaths         = ["/usr/include"]
    , ctxOutputDir            = "build"
    , ctxDefines              = []
    , ctxModules              = mods
    , ctxFailedModules        = failed
    , ctxPreludes             = preludes
    , ctxVerbose              = False
    , ctxModuleGraph          = module_graph
    , ctxIncludes             = includes
    , ctxLastTypeVar          = lastTypeVar
    , ctxTypeVariables        = typeVars
    , ctxTypedDecls           = typedDecls
    , ctxTraitSpecializations = specs
    , ctxImpls                = impls
    , ctxGlobalNames          = globals
    , ctxNoCompile            = False
    , ctxNoLink               = False
    , ctxDumpAst              = False
    , ctxRecursionLimit       = 256
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
    Nothing -> throw $ KitError $ InternalError
      ("Unexpected missing C module: " ++ fp)
      Nothing

makeTypeVar :: CompileContext -> Span -> IO ConcreteType
makeTypeVar ctx pos = do
  last <- readIORef (ctxLastTypeVar ctx)
  let next = last + 1
  writeIORef (ctxLastTypeVar ctx) next
  h_insert (ctxTypeVariables ctx) next (newTypeVarInfo pos)
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

getTraitImpl
  :: CompileContext
  -> TypePath
  -> ConcreteType
  -> IO (Maybe (TraitImplementation Expr (Maybe TypeSpec)))
getTraitImpl ctx trait impl = do
  traitImpls <- h_lookup (ctxImpls ctx) trait
  case traitImpls of
    Just x -> do
      lookup <- h_lookup x impl
      case lookup of
        Just y -> return $ Just y
        _      -> return Nothing
    Nothing -> return Nothing

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
