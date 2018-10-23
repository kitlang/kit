{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
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
  optMainModule :: String,
  optShowVersion :: Bool,
  optVerbose :: Int,
  optTarget :: String,
  optBuildDir :: FilePath,
  optOutputPath :: FilePath,
  optSourcePaths :: [FilePath],
  optIncludePaths :: [FilePath],
  optCompilerPath :: Maybe FilePath,
  optDefines :: [String],
  optIsLibrary :: Bool,
  optNoCompile :: Bool,
  optNoLink :: Bool,
  optDumpAst :: Bool,
  optNoCcache :: Bool,
  optRun :: Bool,
  optNoMangle :: Bool
} deriving (Eq, Show)

options :: Parser Options
options =
  Options
    <$> argument
          str
          (  metavar "MODULE"
          <> help
               "module or file containing the main() function (for binaries) or compilation entry point for libraries"
          )
    <*> switch (long "version" <> help "show the version number and exit")
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
    <*> (optional $ strOption
          (long "cc" <> metavar "PATH" <> help "path to the C compiler")
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
      modules         <- h_new
      stdPath         <- findStd
      baseContext     <- newCompileContext
      defaultIncludes <- defaultIncludePaths
      let
        (mainModule, sourcePaths) = decideModuleAndSourcePaths (s_pack $ optMainModule opts) (optSourcePaths opts)
        ctx = baseContext
          { ctxMainModule   = parseModulePath $ mainModule
          , ctxIsLibrary    = optIsLibrary opts
          , ctxBuildDir     = optBuildDir opts
          , ctxOutputPath   = optOutputPath opts
          , ctxCompilerPath = optCompilerPath opts
          , ctxIncludePaths = optIncludePaths opts ++ defaultIncludes
          , ctxSourcePaths  = sourcePaths ++ stdPath
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
          , ctxNameMangling = not $ optNoMangle opts
          }

      result  <- tryCompile ctx
      endTime <- getCurrentTime
      errors  <- case result of
        Left e -> do
          let errs = flattenErrors e
          mapM_ logError $ errs
          return $ length errs
        Right () -> return 0
      printLog
        $  "total time: "
        ++ (show $ diffUTCTime endTime startTime)
      if errors == 0
        then return ()
        else do
          errorLog $ "compilation failed (" ++ show errors ++ " errors)"
          exitWith $ ExitFailure 1
