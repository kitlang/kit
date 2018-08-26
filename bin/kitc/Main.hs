{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Time
import System.Environment
import System.Exit
import System.IO
import Options.Applicative
import Data.Semigroup ((<>))
import Kit
import Kit.Ast
import Kit.Compiler
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Str

data Options = Options {
  optShowVersion :: Bool,
  optVerbose :: Int,
  optTarget :: String,
  optMainModule :: String,
  optOutputDir :: FilePath,
  optSourcePaths :: [FilePath],
  optIncludePaths :: [FilePath],
  optCompilerPath :: Maybe FilePath,
  optDefines :: [String],
  optIsLibrary :: Bool,
  optNoCompile :: Bool,
  optNoLink :: Bool,
  optDumpAst :: Bool,
  optNoCcache :: Bool,
  optRun :: Bool
} deriving (Eq, Show)

options :: Parser Options
options =
  Options
    <$> switch (long "version" <> help "show the version number and exit")
    <*> (length <$> many
          (flag'
            ()
            (  long "verbose"
            <> short 'v'
            <> help
                 "show extra debugging output; use multiple times (-vv) for even more output"
            )
          )
        )
    <*> strOption
          (  long "target"
          <> short 't'
          <> showDefault
          <> value "c"
          <> metavar "TARGET"
          <> help "compile target (c|web|eval)"
          )
    <*> strOption
          (  long "main"
          <> short 'm'
          <> showDefault
          <> value "main"
          <> metavar "MODULE"
          <> help
               "module containing the main() function (for binaries) or compilation entry point for libraries"
          )
    <*> strOption
          (  long "output"
          <> short 'o'
          <> showDefault
          <> value "build"
          <> metavar "DIR"
          <> help "sets the output directory"
          )
    <*> many sourceDirParser
    <*> many includeDirParser
    <*> (optional $ strOption
          (long "cc" <> metavar "PATH" <> help
            "path to the C compiler"
          )
        )
    <*> many definesParser
    <*> switch (long "lib" <> help "build a library, without linking")
    <*> switch
          (  long "no-compile"
          <> help
               "generates C files and headers but does not compile a library/binary"
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


sourceDirParser = strOption
  (long "src" <> short 's' <> metavar "DIR" <> help "add a source directory")
includeDirParser = strOption
  (long "include" <> short 'I' <> metavar "DIR" <> help
    "add a header include directory"
  )
definesParser = strOption
  (long "define" <> short 'D' <> metavar "KEY[=VAL]" <> help "add a define")

helper' :: Parser (a -> a)
helper' = abortOption ShowHelpText $ mconcat
  [long "help", short 'h', help "show this help text and exit", hidden]

p = prefs (showHelpOnError)

main :: IO ()
main = do
  startTime <- getCurrentTime
  argv      <- getArgs
  let args = if length argv == 0 then ["--help"] else argv

  opts <- handleParseResult $ execParserPure
    p
    (info
      (options <**> helper')
      (  fullDesc
      <> progDesc
           "This is the Kit compiler. It is generally used via the 'kit' build tool."
      <> header ("kitc v" ++ version)
      )
    )
    args

  if optShowVersion opts
    then putStrLn $ "kitc v" ++ version
    else do
      modules <- h_new
      std     <- lookupEnv "KIT_STD_PATH"
      let std_path = case std of
            Just x  -> x
            Nothing -> "std"
      base_context <- newCompileContext
      inc          <- lookupEnv "INCLUDE_PATH"
      let baseIncludePaths = case inc of
            Just x  -> [x]
            Nothing -> ["/usr/include"]
      let ctx = base_context
            { ctxMainModule   = parseModulePath $ s_pack $ optMainModule opts
            , ctxIsLibrary    = optIsLibrary opts
            , ctxOutputDir    = optOutputDir opts
            , ctxCompilerPath = optCompilerPath opts
            , ctxIncludePaths = optIncludePaths opts ++ baseIncludePaths
            , ctxSourcePaths  = optSourcePaths opts ++ [std_path]
            , ctxDefines      = map
              (\s -> (takeWhile (/= '=') s, drop 1 $ dropWhile (/= '=') s))
              (optDefines opts)
            , ctxModules      = modules
            , ctxVerbose      = optVerbose opts
            , ctxNoCompile    = optNoCompile opts
            , ctxNoLink       = optNoLink opts
            , ctxDumpAst      = optDumpAst opts
            , ctxNoCcache     = optNoCcache opts
            , ctxRun          = optRun opts
            }

      result  <- tryCompile ctx
      endTime <- getCurrentTime
      status  <- case result of
        Left e -> do
          mapM_ logError $ flattenErrors e
          return 1
        Right () -> return 0
      printLog
        $  "total compile time: "
        ++ (show $ diffUTCTime endTime startTime)
      if status == 0
        then return ()
        else do
          errorLog $ "compilation failed"
          exitWith $ ExitFailure status
