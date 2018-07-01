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

  findSourceFile :: FilePath -> [FilePath] -> IO (Maybe FilePath)
  findSourceFile f [] = do return Nothing
  findSourceFile f (h:t) = do
    let f' = h </> f
    exists <- doesFileExist f'
    if exists
      then return $ Just f'
      else findSourceFile f t

  debugLog :: CompileContext -> String -> IO ()
  debugLog ctx msg = do
    if context_verbose ctx
      then logMsg Debug msg
      else return ()

  findModule :: CompileContext -> ModulePath -> Maybe Span -> IO FilePath
  findModule ctx mod pos = do
    let modPath = moduleFilePath mod
    debugLog ctx $ "searching for module <" ++ s_unpack (showModulePath mod) ++ ">"
    match <- findSourceFile modPath (context_source_paths ctx)
    case match of
      Just f -> do
        debugLog ctx $ "found module <" ++ s_unpack (showModulePath mod) ++ "> at " ++ f
        return f
      Nothing -> do
        throw $ Errs $ [
          errp ImportError ("Couldn't find module <" ++ s_unpack (showModulePath mod) ++
                            ">; tried searching the following locations: \n\n" ++
                            (intercalate "\n" ["  - " ++ (dir </> modPath) | dir <- context_source_paths ctx]))
               pos
          ]
