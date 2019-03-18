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
  ldFlags :: [String],
  kitFlags :: [String]
} deriving (Show, Eq)

newToolchain :: String -> [(String, String)] -> IO Toolchain
newToolchain name defines = do
  cc           <- lookupEnv "CC"
  includePaths <- lookupEnv "INCLUDE_PATHS"
  cFlags       <- lookupEnv "CFLAGS"
  cppFlags     <- lookupEnv "CPPFLAGS"
  ldFlags      <- lookupEnv "LDFLAGS"
  kitFlags     <- lookupEnv "KITFLAGS"
  return $ Toolchain
    { toolchainName  = name
    , ccPath         = fromMaybe "" cc
    , ccIncludePaths = wordsBy isSpace $ fromMaybe "" includePaths
    , cppFlags       = (wordsBy isSpace $ fromMaybe "" cppFlags)
      ++ [ "-D" ++ a ++ "=" ++ b | (a, b) <- defines ]
    , cFlags         = wordsBy isSpace $ fromMaybe "" cFlags
    , ldFlags        = wordsBy isSpace $ fromMaybe "" ldFlags
    , kitFlags       = wordsBy isSpace $ fromMaybe "" kitFlags
    }

toolchainSearchPaths :: String -> IO [FilePath]
toolchainSearchPaths x | x == "linux" || x == "darwin" = do
  defaults <- (toolchainSearchPaths "")
  return $ defaults ++ ["/etc/kitlang/toolchains"]
toolchainSearchPaths _ = do
  override <- lookupEnv "KIT_TOOLCHAIN_PATH"
  exePath  <- getExecutablePath
  let exeDir = takeDirectory exePath
  return $ catMaybes [override] ++ [".", "toolchains", exeDir </> "toolchains"]

loadToolchain :: String -> [(String, String)] -> IO Toolchain
loadToolchain name defines = do
  -- if this is a path to a toolchain, use that
  path   <- canonicalizePath $ name
  exists <- doesFileExist path
  if exists
    then loadToolchainFile name defines
    else do
      dirs  <- toolchainSearchPaths os
      paths <- forM dirs $ \dir -> canonicalizePath $ dir </> name
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
        Just f  -> loadToolchainFile f defines
        Nothing -> throwk $ BasicError ("Unknown toolchain: " ++ name) Nothing

defaultToolchain = case os of
  "darwin"  -> "darwin-gcc"
  "mingw32" -> "windows-mingw"
  _         -> "linux-gcc"

loadToolchainFile :: FilePath -> [(String, String)] -> IO Toolchain
loadToolchainFile fp defines = do
  env       <- parseFile fp
  toolchain <- newToolchain fp defines
  overrides <-
    forM ["CC", "INCLUDE_PATHS", "CFLAGS", "CPPFLAGS", "LDFLAGS"] $ \var -> do
      value <- lookupEnv var
      return $ case value of
        Just x  -> Just (var, x)
        Nothing -> Nothing
  return $ foldl
    (\toolchain (key, value) -> case key of
      "CC"            -> toolchain { ccPath = value }
      "INCLUDE_PATHS" -> toolchain
        { ccIncludePaths = ccIncludePaths toolchain ++ splitOn " " value
        }
      "CFLAGS" -> toolchain { cFlags = cFlags toolchain ++ splitOn " " value }
      "CPPFLAGS" ->
        toolchain { cppFlags = cppFlags toolchain ++ splitOn " " value }
      "LDFLAGS" ->
        toolchain { ldFlags = ldFlags toolchain ++ splitOn " " value }
      "KITFLAGS" ->
        toolchain { kitFlags = kitFlags toolchain ++ splitOn " " value }
      _ -> toolchain
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

getCppFlags toolchain includeCflags =
  cppFlags toolchain
    ++ [ "-I" ++ dir | dir <- ccIncludePaths toolchain ]
    ++ (if includeCflags then cFlags toolchain else [])
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
  let args = (getCppFlags toolchain True) ++ ["-c", "-o" ++ out, fp]
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
  -> Bool
  -> FilePath
  -> IO FilePath
invokeLinker toolchain logger shared files source out = do
  let args =
        (if source then getCppFlags toolchain False else [])
          ++ files
          ++ (getLdFlags toolchain)
          ++ (if shared then ["-shared"] else [])
          ++ ["-o" ++ out]
  logger $ showCommandForUser (ccPath toolchain) args
  callProcess (ccPath toolchain) args
  binPath <- canonicalizePath out
  return $ binPath
