{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.Time
import System.Environment
import System.Exit
import System.Info
import System.IO
import System.Process
import Options.Applicative
import Data.Semigroup ((<>))
import Kit
import Kit.Ast
import Kit.Compiler
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Str
import Kit.Toolchain

data Options = Options {
  optMainModule :: String,
  optVerbose :: Int,
  optQuiet :: Bool,
  optTarget :: String,
  optBuildDir :: FilePath,
  optOutputPath :: FilePath,
  optSourcePaths :: [FilePath],
  optIncludePaths :: [FilePath],
  optCompilerFlags :: [String],
  optLinkerFlags :: [String],
  optBuild :: Maybe String,
  optHost :: Maybe String,
  optDefines :: [String],
  optIsLibrary :: Bool,
  optNoCompile :: Bool,
  optNoLink :: Bool,
  optDumpAst :: Bool,
  optNoCcache :: Bool,
  optRun :: Bool,
  optNoMangle :: Bool,
  optShowEnv :: Bool
} deriving (Eq, Show)

options :: Parser Options
options =
  Options
    <$> argument
          str
          (  metavar "MODULE"
          <> help
               "module or file containing the main() function (for binaries) or compilation entry point for libraries"
          <> value ""
          )
    <*> (length <$> many
          (flag'
            ()
            (  long "verbose"
            <> short 'v'
            <> help
                 "show extra debugging output; can be used multiple times (-vvv) for even more output"
            )
          )
        )
    <*> switch (long "quiet" <> short 'q' <> help "show less output")
    <*> strOption
          (  long "target"
          <> short 't'
          <> showDefault
          <> value "c"
          <> metavar "TARGET"
          <> help "compile target (c|web|eval)"
          )
    <*> strOption
          (  long "build-dir"
          <> showDefault
          <> value "build"
          <> metavar "DIR"
          <> help "sets the path to the build directory"
          )
    <*> strOption
          (  long "output"
          <> short 'o'
          <> showDefault
          <> value "main"
          <> metavar "FILE"
          <> help "sets the output path"
          )
    <*> many sourceDirParser
    <*> many includeDirParser
    <*> many compilerFlagParser
    <*> many linkerFlagParser
    <*> (optional $ strOption
          (long "build" <> metavar "TOOLCHAIN" <> help "name, or path to, the native toolchain")
        )
    <*> (optional $ strOption
        (long "host" <> metavar "TOOLCHAIN" <> help "name, or path to, the host toolchain; used for cross-compilation")
      )
    <*> many definesParser
    <*> switch (long "lib" <> help "build a shared library, not an executable")
    <*> switch
          (  long "no-compile"
          <> help
               "generates C files and headers but does not compile a library or executable (implies --no-link)"
          )
    <*> switch
          (long "no-link" <> help
            "compile C files but skip linking into a library or binary"
          )

    <*> switch
          (long "dump-ast" <> help
            "output the typed AST after typing, for debugging purposes"
          )
    <*> switch
          (  long "no-ccache"
          <> help
               "don't use ccache for compilation, even if it is available on your path"
          )
    <*> switch
          (long "run" <> help "run the program after successful compilation")
    <*> switch (long "no-mangle" <> help "disables name mangling")
    <*> switch
          (long "env" <> help "show environment info and exit without compiling"
          )

compilerFlagParser = strOption
  (long "compile-flag" <> short 'c' <> metavar "FLAG" <> help
    "add a compiler flag; can be repeated"
  )
linkerFlagParser = strOption
  (long "linker-flag" <> short 'l' <> metavar "FLAG" <> help
    "add a linker flag; can be repeated"
  )
sourceDirParser = strOption
  (long "src" <> short 's' <> metavar "DIR" <> help
    "add a source directory; can be repeated"
  )
includeDirParser = strOption
  (long "include" <> short 'I' <> metavar "DIR" <> help
    "add a header include directory; can be repeated"
  )
definesParser = strOption
  (long "define" <> short 'D' <> metavar "KEY[=VAL]" <> help
    "add a define; can be repeated"
  )

helper' :: Parser (a -> a)
helper' = abortOption ShowHelpText $ mconcat
  [long "help", short 'h', help "show this help text and exit", hidden]

version' :: Parser (a -> a)
version' = abortOption (InfoMsg $ "kitc v" ++ version)
  $ mconcat [long "version", help "show version and exit"]

p = prefs (showHelpOnEmpty)

main :: IO ()
main = do
  args <- getArgs
  let
    parseOpts args = handleParseResult $ execParserPure
      p
      (info
        (options <**> helper' <**> version')
        (  fullDesc
        <> progDesc
             "This is the Kit compiler. It is generally used via the 'kit' build tool."
        <> header ("kitc v" ++ version)
        )
      )
      args
  opts        <- parseOpts args

  stdPath     <- findStd
  buildToolchain <- lookupEnv "KIT_BUILD_TOOLCHAIN"
  hostToolchain <- lookupEnv "KIT_HOST_TOOLCHAIN"
  cc <- try (do
    build       <- loadToolchain $ fromMaybe defaultToolchain $ optBuild opts <|> buildToolchain
    host        <- case optHost opts of {Just t -> loadToolchain t; Nothing -> case hostToolchain of
                                                                                  Just e -> loadToolchain e
                                                                                  Nothing -> return build}
    return (build, host)) :: IO (Either KitError (Toolchain, Toolchain))
  (build, host) <- case cc of
    Left e -> do
      showErrs e
      exitWith $ ExitFailure 1
    Right x -> return x
  [build, host] <- forM [build, host] $ \cc -> return $ cc {
    ccIncludePaths = ccIncludePaths cc ++ optIncludePaths opts,
    cFlags = cFlags cc ++ optCompilerFlags opts,
    ldFlags = ldFlags cc ++ optLinkerFlags opts
  }
  baseContext <- newCompileContext
  let (mainModule, sourcePaths) = decideModuleAndSourcePaths
        (s_pack $ optMainModule opts)
        (optSourcePaths opts)
      ctx = baseContext
        { ctxMainModule    = parseModulePath $ mainModule
        , ctxIsLibrary     = optIsLibrary opts
        , ctxBuildDir      = optBuildDir opts
        , ctxOutputPath    = optOutputPath opts
        , ctxSourcePaths   = map parseSourcePath $ sourcePaths ++ stdPath
        , ctxDefines       = map
          (\s -> (takeWhile (/= '=') s, drop 1 $ dropWhile (/= '=') s))
          (optDefines opts)
        , ctxVerbose       = if optQuiet opts then -1 else optVerbose opts
        , ctxNoCompile     = optNoCompile opts
        , ctxNoLink        = optNoLink opts
        , ctxDumpAst       = optDumpAst opts
        , ctxNoCcache      = optNoCcache opts
        , ctxRun           = optRun opts
        , ctxNameMangling  = not $ optNoMangle opts
        }

  if optShowEnv opts
    then do
      printLog "kitc version"
      printDebugLog version
      printLog "OS"
      printDebugLog os
      printLog "Source paths"
      printDebugLog $ show
        [ path
            ++ (if null anchor
                 then ""
                 else (":" ++ s_unpack (showModulePath anchor))
               )
        | (path, anchor) <- ctxSourcePaths ctx
        ]
      printLog "Standard prelude location"
      preludePath <-
        (try (findModule ctx ["prelude"] Nothing)) :: IO
          (Either KitError FilePath)
      case preludePath of
        Left  e -> showErrs e >> return ()
        Right f -> printDebugLog f
      let toolchains = if build == host then [("COMPILER", build)] else [("BUILD", build), ("HOST", host)]
      forM_ toolchains $ \(ccType, cc) -> do
        printLog $ "** " ++ ccType ++ " **"
        printLog "Toolchain"
        printDebugLog $ toolchainName cc
        printLog "Compiler"
        printDebugLog $ ccPath cc
        ccVersionResult <- (try $ readProcess (ccPath cc) ["--version"] "") :: IO (Either IOError String)
        case ccVersionResult of
          Left err -> errorLog $ "Error getting compiler version:\n\n" ++ show err
          Right s -> putStr s
        printLog "Include paths"
        printDebugLog $ show (ccIncludePaths cc)
        printLog "Compiler flags"
        printDebugLog $ show $ getCppFlags cc
        printLog "Linker flags"
        printDebugLog $ show $ getLdFlags cc

    else do
      when (null $ optMainModule opts) $ do
        errorLog $ "no main module specified"
        parseOpts ["--help"]
        exitWith $ ExitFailure 1
      result <- tryCompile ctx build host
      errors <- case result of
        Left  e  -> showErrs e
        Right () -> return 0

      if errors == 0
        then return ()
        else do
          errorLog $ "compilation failed (" ++ show errors ++ " errors)"
          exitWith $ ExitFailure 1

parseSourcePath :: String -> (FilePath, ModulePath)
parseSourcePath (':' : ':' : t) = ("", parseModulePath $ s_pack t)
parseSourcePath (h         : t) = let (a, b) = parseSourcePath t in (h : a, b)
parseSourcePath []              = ("", [])
