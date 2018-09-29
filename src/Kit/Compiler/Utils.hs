module Kit.Compiler.Utils where

import Control.Exception
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import Kit.Ast
import Kit.Compiler.Context
import Kit.Error
import Kit.Log
import Kit.Parser
import Kit.Str

data ImportError = ImportError ModulePath [FilePath] (Maybe Span) deriving (Eq, Show)
instance Errable ImportError where
  logError e@(ImportError mod searchPaths _) =
    logErrorBasic e $ "Couldn't find module <"
                                ++ s_unpack (showModulePath mod)
                                ++ ">; tried searching the following locations: \n\n"
                                ++ (intercalate
                                    "\n"
                                    [ "  - " ++ s | s <- searchPaths]
                                  )
  errPos (ImportError _ _ pos) = pos

findSourceFile :: FilePath -> [FilePath] -> IO (Maybe FilePath)
findSourceFile f [] = do
  return Nothing
findSourceFile f (h : t) = do
  let f' = h </> f
  exists <- doesFileExist f'
  if exists then return $ Just f' else findSourceFile f t

debugLog :: CompileContext -> String -> IO ()
debugLog ctx msg = do
  when (ctxVerbose ctx > 0) $ logMsg (Just Debug) msg

noisyDebugLog :: CompileContext -> String -> IO ()
noisyDebugLog ctx msg = do
  when (ctxVerbose ctx > 1) $ logMsg (Just Debug) msg

veryNoisyDebugLog :: CompileContext -> String -> IO ()
veryNoisyDebugLog ctx msg = do
  when (ctxVerbose ctx > 2) $ logMsg (Just Debug) msg

findModule :: CompileContext -> ModulePath -> Maybe Span -> IO FilePath
findModule ctx mod pos = do
  let modPath = moduleFilePath mod
  match <- findSourceFile modPath (ctxSourcePaths ctx)
  case match of
    Just f -> do
      debugLog ctx
        $  "found module <"
        ++ s_unpack (showModulePath mod)
        ++ "> at "
        ++ f
      return f
    Nothing -> do
      throw $ KitError $ ImportError
        mod
        [ (dir </> modPath) | dir <- ctxSourcePaths ctx ]
        pos

plural 1 = ""
plural _ = "s"
