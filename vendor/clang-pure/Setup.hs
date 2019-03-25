{-|
Module      : Main
Description : Setup script for clang-pure build
Copyright   : (C) Richard Cook, Patrick Chilton 2016
Licence     : Apache 2.0
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This setup script adds a configuration hook to the build process to discover
the LLVM library and include paths from the standard system-wide installation
location or from an alternative location if overridden with both the
@CLANG_PURE_LLVM_LIB_DIR@ and @CLANG_PURE_LLVM_INCLUDE_DIR@ environment variables.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Configure
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Environment
import System.IO.Error
import System.Process
import qualified Data.Version as Version
import Text.ParserCombinators.ReadP

data LLVMPathInfo = LLVMPathInfo
  { llvmLibraryDir :: FilePath
  , llvmIncludeDir :: FilePath
  } deriving (Eq, Ord, Show)

llvmLibDirEnvVarName :: String
llvmLibDirEnvVarName = "CLANG_PURE_LLVM_LIB_DIR"

llvmIncludeDirEnvVarName :: String
llvmIncludeDirEnvVarName = "CLANG_PURE_LLVM_INCLUDE_DIR"

minVersion :: Version.Version
minVersion = Version.makeVersion [ 3, 8, 0 ]

#if !(MIN_VERSION_Cabal(2, 0, 0))
die' :: Verbosity -> String -> IO a
die' _ = die
#endif

findLLVMConfigPaths :: Verbosity -> IO LLVMPathInfo
findLLVMConfigPaths verbosity = do
  let llvmConfigCandidates =
        "llvm-config" :
          [ "llvm-config-" ++ show major ++ "." ++ show minor
          | major <- [9,8..3 :: Int]
          , minor <- [9,8..0 :: Int]
          ]
  let tryCandidates [] = die' verbosity $ "Could not find llvm-config with minimum version " ++ Version.showVersion minVersion ++ "."
      tryCandidates (llvmConfig : candidates) = do
        llvmConfigResult <- tryJust
          (guard . isDoesNotExistError)
          (readProcess llvmConfig ["--version", "--libdir", "--includedir"] "")
        case llvmConfigResult of
          Left _ -> do
            putStrLn ("Could not execute " ++ llvmConfig)
            tryCandidates candidates
          Right llvmConfigOutput -> do
            putStrLn ("Found " ++ llvmConfig)
            case lines llvmConfigOutput of
              [ versionString, libraryDir, includeDir ] ->
                case readP_to_S (Version.parseVersion <* eof) (takeWhile (\char -> isJust $ elemIndex char "0123456789.") versionString) of
                  [ ( version, _ ) ]
                    | version >= minVersion -> return $ LLVMPathInfo libraryDir includeDir
                    | otherwise -> do
                      putStrLn ("Version too low: " ++ show version)
                      tryCandidates candidates
                  _ -> die' verbosity "Couldn't parse llvm-config version string."
              _ -> die' verbosity "Unexpected llvm-config output."
  tryCandidates llvmConfigCandidates

getLLVMPathInfo :: Verbosity -> IO LLVMPathInfo
getLLVMPathInfo verbosity = do
  m'llvmLibDirEnvVar <- lookupEnv llvmLibDirEnvVarName
  m'llvmIncludeDirEnvVar <- lookupEnv llvmIncludeDirEnvVarName
  case (m'llvmLibDirEnvVar, m'llvmIncludeDirEnvVar) of
    ( Just libraryDir, Just includeDir ) -> return $ LLVMPathInfo libraryDir includeDir
    _ -> findLLVMConfigPaths verbosity

clangPureConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
clangPureConfHook (d, bi) flags = do
  localBuildInfo <- confHook simpleUserHooks (d, bi) flags
  let pd = localPkgDescr localBuildInfo

  -- see if it just works (eg on Nix)
  mbError <- try $ checkForeignDeps pd localBuildInfo normal

  let verbosity = fromFlagOrDefault normal (configVerbosity flags)

  let addCSources libBuildInfo =
        libBuildInfo
#if !(MIN_VERSION_GLASGOW_HASKELL(8, 2, 1, 0) && MIN_VERSION_inline_c(0, 6, 0))
          { cSources = -- define the generated c-sources here so that they don't get picked up by sdist
#if defined(mingw32_HOST_OS) && !(MIN_VERSION_GLASGOW_HASKELL(8, 0, 2, 0))
              ["srcLanguageCClangInternalFFI.c"] -- work around a bug in hsc2hs
#else
              ["src/Language/C/Clang/Internal/FFI.c"]
#endif
          }
#endif

  addDirs <- case mbError of
    Right () -> return id
    Left (_ :: SomeException) -> do
      warn verbosity "Couldn't find libclang, attempting to find it with llvm-config or environment variables..."

      LLVMPathInfo{..} <- getLLVMPathInfo verbosity

      return $ \libBuildInfo -> libBuildInfo
        { includeDirs = llvmIncludeDir : includeDirs libBuildInfo
        , extraLibDirs = llvmLibraryDir : extraLibDirs libBuildInfo
        }

  let Just lib = library pd
  let lbi = libBuildInfo lib
  return localBuildInfo
    { localPkgDescr = pd
      { library = Just lib { libBuildInfo = addDirs $ addCSources lbi }
      }
    }

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = clangPureConfHook }
