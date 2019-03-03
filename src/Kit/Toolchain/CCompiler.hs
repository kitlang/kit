module Kit.Toolchain.CCompiler where

import Configuration.Dotenv
import Control.Exception
import Control.Monad
import Data.Char
import Data.Mutable
import Data.List
import Data.List.Split
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Info
import System.Process
import Text.Regex.Posix
import Kit.Ast
import Kit.Error
import Kit.Log
import Kit.Str

data Toolchain = Toolchain {
  toolchainName :: String,
  ccPath :: String,
  ccIncludePaths :: [FilePath],
  cppFlags :: [String],
  cFlags :: [String],
  ldFlags :: [String]
} deriving (Show, Eq)

newToolchain name = do
  cc           <- lookupEnv "CC"
  includePaths <- lookupEnv "INCLUDE_PATHS"
  cFlags       <- lookupEnv "CFLAGS"
  cppFlags     <- lookupEnv "CPPFLAGS"
  ldFlags      <- lookupEnv "LDFLAGS"
  return $ Toolchain
    { toolchainName  = name
    , ccPath         = fromMaybe "" cc
    , ccIncludePaths = wordsBy isSpace $ fromMaybe "" includePaths
    , cppFlags       = wordsBy isSpace $ fromMaybe "" cppFlags
    , cFlags         = wordsBy isSpace $ fromMaybe "" cFlags
    , ldFlags        = wordsBy isSpace $ fromMaybe "" ldFlags
    }

toolchainSearchPaths :: String -> [FilePath]
toolchainSearchPaths x | x == "linux" || x == "darwin" =
  (toolchainSearchPaths "") ++ ["/etc/kitlang/toolchains"]
toolchainSearchPaths _ = [".", "toolchains"]

loadToolchain :: String -> IO Toolchain
loadToolchain name = do
  -- if this is a path to a toolchain, use that
  path   <- canonicalizePath $ name
  exists <- doesFileExist path
  if exists
    then loadToolchainFile name
    else do
      paths <- forM (toolchainSearchPaths os) $ \dir -> canonicalizePath $ dir </> name
      found <- foldM
        (\acc path -> case acc of
          Just x  -> return acc
          Nothing -> do
            path   <- canonicalizePath path
            exists <- doesFileExist path
            return $ if exists then Just path else Nothing
        )
        Nothing
        paths
      case found of
        Just f  -> loadToolchainFile f
        Nothing -> throwk $ BasicError ("Unknown toolchain: " ++ name) Nothing

defaultToolchain = case os of
  "darwin"  -> "darwin-gcc"
  "windows" -> "windows-gcc"
  _         -> "linux-gcc"

loadToolchainFile :: FilePath -> IO Toolchain
loadToolchainFile fp = do
  env       <- parseFile fp
  toolchain <- newToolchain fp
  overrides <-
    forM ["CC", "INCLUDE_PATHS", "CFLAGS", "CPPFLAGS", "LDFLAGS"] $ \var -> do
      value <- lookupEnv var
      return $ case value of
        Just x  -> Just (var, x)
        Nothing -> Nothing
  return $ foldl
    (\toolchain (key, value) -> case key of
      "CC"            -> toolchain { ccPath = value }
      "INCLUDE_PATHS" -> toolchain { ccIncludePaths = splitOn " " value }
      "CFLAGS"        -> toolchain { cFlags = splitOn " " value }
      "CPPFLAGS"      -> toolchain { cppFlags = splitOn " " value }
      "LDFLAGS"       -> toolchain { ldFlags = splitOn " " value }
      _               -> toolchain
    )
    toolchain
    (env ++ catMaybes overrides)

findIncludePaths :: FilePath -> IO [FilePath]
findIncludePaths fp = do
  (_, _, stderr) <- readCreateProcessWithExitCode
    (shell $ fp ++ " -E -Wp,-v -")
    ""
  return $ parseIncludePaths (lines stderr) False []

parseIncludePaths :: [String] -> Bool -> [FilePath] -> [FilePath]
parseIncludePaths []      _     paths = paths
parseIncludePaths (h : t) False paths = parseIncludePaths
  t
  (h =~ ("#include (\"|<)...(\"|>) search starts here:" :: String) :: Bool)
  paths
parseIncludePaths ("End of search list." : t) True paths =
  parseIncludePaths t False paths
parseIncludePaths ((' ' : h) : t) True paths =
  parseIncludePaths t True (h : paths)
parseIncludePaths (_ : t) True paths = parseIncludePaths t True paths

getCppFlags toolchain =
  cppFlags toolchain
    ++ [ "-I" ++ dir | dir <- ccIncludePaths toolchain ]
    ++ cFlags toolchain
getLdFlags toolchain = cFlags toolchain ++ ldFlags toolchain

findCcache :: IO (Maybe FilePath)
findCcache = findExecutable "ccache"

invokeCompiler
  :: Toolchain
  -> (String -> IO ())
  -> Bool
  -> FilePath
  -> FilePath
  -> IO (Either String String)
invokeCompiler toolchain logger useCcache fp out = do
  let args = (getCppFlags toolchain) ++ ["-c", "-o" ++ out, fp]
  ccache         <- if useCcache then findCcache else return Nothing
  (ccPath, args) <- return $ case ccache of
    Just x  -> (x, ccPath toolchain : args)
    Nothing -> (ccPath toolchain, args)
  logger $ showCommandForUser ccPath args
  (status, _, stderr) <- readCreateProcessWithExitCode (proc ccPath args) ""
  if status == ExitSuccess then return $ Right stderr else return $ Left stderr

invokeLinker
  :: Toolchain
  -> (String -> IO ())
  -> Bool
  -> [FilePath]
  -> FilePath
  -> IO FilePath
invokeLinker toolchain logger shared files out = do
  let args =
        files
          ++ (getLdFlags toolchain)
          ++ (if shared then ["-shared"] else [])
          ++ ["-o" ++ out]
  logger $ showCommandForUser (ccPath toolchain) args
  callProcess (ccPath toolchain) args
  binPath <- canonicalizePath out
  return $ binPath
