module Kit.Compiler.Utils where

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import Kit.Ast
import Kit.Compiler.Context
import Kit.Error
import Kit.Log
import Kit.Parser
import Kit.Str
import Kit.Toolchain

printLogIf ctx s = when (ctxVerbose ctx >= 0) $ printLog s

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
debugLog ctx msg = when (ctxVerbose ctx > 0) $ logMsg (Just Debug) msg

noisyDebugLog :: CompileContext -> String -> IO ()
noisyDebugLog ctx msg = when (ctxVerbose ctx > 1) $ logMsg (Just Debug) msg

veryNoisyDebugLog :: CompileContext -> String -> IO ()
veryNoisyDebugLog ctx msg = when (ctxVerbose ctx > 2) $ logMsg (Just Debug) msg

findSourceModule
  :: ModulePath -> [(FilePath, ModulePath)] -> IO (Maybe FilePath)
findSourceModule m []      = return Nothing
findSourceModule m (h : t) = do
  next <- findSourceModule m t
  case prefixedModPath m h of
    Just x -> do
      exists <- doesFileExist x
      return $ if exists then Just x else next
    Nothing -> return next

prefixedModPath :: ModulePath -> (FilePath, ModulePath) -> Maybe FilePath
prefixedModPath [] _       = Nothing
prefixedModPath m  (d, []) = Just $ d </> moduleFilePath m
prefixedModPath (m : n) (d, (a : b)) =
  if m == a then prefixedModPath n (d, b) else Nothing
-- prefixedModPath _ _ = Nothing

findModule :: CompileContext -> ModulePath -> Maybe Span -> IO FilePath
findModule ctx mod pos = do
  match <- findSourceModule mod (ctxSourcePaths ctx)
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
        (catMaybes
          [ prefixedModPath mod (dir, prefix)
          | (dir, prefix) <- ctxSourcePaths ctx
          ]
        )
        pos

plural 1 = ""
plural _ = "s"
