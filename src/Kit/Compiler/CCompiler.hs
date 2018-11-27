module Kit.Compiler.CCompiler where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath
import System.Info
import System.Process
import Text.Regex.Posix
import Kit.Ast
import Kit.Error
import Kit.Log
import Kit.Str

data CompilerType
  = Gcc
  | Clang
  | Unknown
  deriving (Show, Eq)

data CCompiler = CCompiler {
                            ccPath :: String,
                            ccType :: CompilerType,
                            ccIncludePaths :: [FilePath],
                            ccFlags :: [String]
                          } deriving (Show)

findIncludePaths :: CompilerType -> FilePath -> IO [FilePath]
findIncludePaths ccType fp | ccType == Gcc || ccType == Clang = do
  (_, _, stderr) <- readCreateProcessWithExitCode
    (shell $ fp ++ " -E -Wp,-v -")
    ""
  return $ parseIncludePaths (lines stderr) False []
findIncludePaths ccType fp = do
  return []

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

getCompiler :: Maybe FilePath -> IO CCompiler
getCompiler fp = do
  path <- case fp of
    Just fp -> return fp
    Nothing -> findCompilerPath
  let ccType = inferCcType path
  includePaths <- findIncludePaths ccType path
  let flags = defaultFlags ccType
  return $ CCompiler
    { ccPath         = path
    , ccType         = ccType
    , ccIncludePaths = includePaths
    , ccFlags        = flags
    }

inferCcType :: FilePath -> CompilerType
inferCcType path =
  let cc = takeBaseName path
  in  case takeBaseName path of
        _ | isPrefixOf "gcc" cc -> Gcc
        _ | isPrefixOf "clang" cc -> Clang
        _ -> throwk $ BasicError ("Unsupported compiler: " ++ path) Nothing

findCompilerPath :: IO FilePath
findCompilerPath = do
  -- use a CC environment variable if it exists
  ccEnv <- lookupEnv "CC"
  case ccEnv of
    Just cc -> return cc
    Nothing -> do
      -- look for a compiler next to kitc
      let exes = ["gcc", "clang"]
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

defaultFlags :: CompilerType -> [String]
defaultFlags cc =
  [ "-D_GNU_SOURCE"
    , "-D_BSD_SOURCE"
    , "-D_DEFAULT_SOURCE"
    , "-std=c99"
    , "-pedantic"
    , "-O3"
    , "-Os"
    ]
    ++ osSpecificDefaultCompileArgs os
    ++ ccSpecificDefaultCompileArgs cc

osSpecificDefaultCompileArgs "darwin" =
  [ "-U__BLOCKS__"
  , "-Wno-expansion-to-defined"
  , "-Wno-gnu-zero-variadic-macro-arguments"
  ]
osSpecificDefaultCompileArgs _ = []

ccSpecificDefaultCompileArgs Gcc   = ["-Wno-missing-braces"]
ccSpecificDefaultCompileArgs Clang = ["-Wno-unused-command-line-argument"]
ccSpecificDefaultCompileArgs _     = []
