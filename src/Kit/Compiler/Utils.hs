module Kit.Compiler.Utils where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import System.Directory
import System.FilePath
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Error
import Kit.HashTable
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
  when (ctxVerbose ctx) $ logMsg (Just Debug) msg

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

validName :: Str -> Str
validName name = if s_length name > 32 then s_concat ["kit", s_hash name] else name

mangleName :: [Str] -> Str -> Str
mangleName [] s = s
mangleName namespace s = validName $ s_join "_" (("kit" : namespace) ++ [s])

monomorphName :: Str -> [ConcreteType] -> Str
monomorphName name p = s_concat [name, "__", monomorphSuffix p]

monomorphSuffix :: [ConcreteType] -> Str
monomorphSuffix p = s_hash $ s_concat (map (s_pack . show) p)
