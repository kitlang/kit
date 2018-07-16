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
import Kit.Compiler.TypedDecl
import Kit.Error
import Kit.HashTable
import Kit.Ir
import Kit.Parser.Span
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
  ctxIncludes :: IORef [FilePath],
  ctxLastTypeVar :: IORef Int,
  ctxTypeVariables :: HashTable Int TypeVarInfo,
  ctxTypedDecls :: HashTable TypePath TypedDecl
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
  mods         <- h_new
  failed       <- h_new
  preludes     <- h_new
  typedDecls   <- h_new
  includes     <- newIORef []
  lastTypeVar  <- newIORef 0
  typeVars     <- h_new
  return $ CompileContext
    { ctxMainModule    = ["main"]
    , ctxSourcePaths   = ["src"]
    , ctxIncludePaths  = ["/usr/include"]
    , ctxOutputDir     = "build"
    , ctxDefines       = []
    , ctxModules       = mods
    , ctxFailedModules = failed
    , ctxPreludes      = preludes
    , ctxVerbose       = False
    , ctxModuleGraph   = module_graph
    , ctxIncludes      = includes
    , ctxLastTypeVar   = lastTypeVar
    , ctxTypeVariables = typeVars
    , ctxTypedDecls    = typedDecls
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
    Nothing -> throw $ Errs
      [ err InternalError $ "Unexpected missing module: " ++ s_unpack
          (showModulePath mod)
      ]

getCMod :: CompileContext -> FilePath -> IO Module
getCMod ctx fp = do
  let modPath = includeToModulePath fp
  m <- h_lookup (ctxModules ctx) modPath
  case m of
    Just m' -> return m'
    Nothing ->
      throw $ Errs [err InternalError $ "Unexpected missing C module: " ++ fp]

makeTypeVar :: CompileContext -> Span -> IO ConcreteType
makeTypeVar ctx pos = do
  last <- readIORef (ctxLastTypeVar ctx)
  let next = last + 1
  writeIORef (ctxLastTypeVar ctx) next
  h_insert (ctxTypeVariables ctx) next (newTypeVarInfo pos)
  return $ TypeTypeVar (TypeVar next)

getTypeVar :: CompileContext -> Int -> IO TypeVarInfo
getTypeVar ctx tv = h_get (ctxTypeVariables ctx) tv

resolveVar
  :: CompileContext -> [Scope Binding] -> Module -> Str -> IO (Maybe Binding)
resolveVar ctx scopes mod s = do
  local <- resolveBinding scopes s
  case local of
    Just _  -> return local
    Nothing -> do
      imports <- mapM (getMod ctx) (map fst $ modImports mod)
      resolveBinding (map modVars (mod : imports)) s

getModImports :: CompileContext -> Module -> IO [Module]
getModImports ctx mod = do
  imports  <- mapM (getMod ctx) (map fst $ modImports mod)
  includes <- mapM (getCMod ctx) (map fst $ modIncludes mod)
  return $ mod : (imports ++ includes)
