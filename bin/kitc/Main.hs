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
  opt_show_version :: Bool,
  opt_verbose :: Bool,
  opt_target :: String,
  opt_main_module :: String,
  opt_output_dir :: FilePath,
  opt_source_paths :: [FilePath],
  opt_include_paths :: [FilePath],
  opt_defines :: [String]
} deriving (Eq, Show)

options :: Parser Options
options =
  Options
    <$> switch
          (long "version" <> short 'v' <> help
            "show the version number and exit"
          )
    <*> switch (long "verbose" <> help "show extra debugging output")
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
          <> help "module path containing main() function"
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
    <*> many definesParser

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

  if opt_show_version opts
    then putStrLn $ "kitc v" ++ version
    else do
      modules <- h_new
      std     <- lookupEnv "KIT_STD_PATH"
      let std_path = case std of
            Just x  -> x
            Nothing -> "std"
      base_context <- newCompileContext
      let ctx = base_context
            { ctxMainModule   = parseModulePath $ s_pack $ opt_main_module opts
            , ctxOutputDir    = opt_output_dir opts
            , ctxIncludePaths = opt_include_paths opts ++ ["/usr/include"]
            , ctxSourcePaths  = opt_source_paths opts ++ [std_path]
            , ctxDefines      = map
              (\s -> (takeWhile (/= '=') s, drop 1 $ dropWhile (/= '=') s))
              (opt_defines opts)
            , ctxModules      = modules
            , ctxVerbose      = opt_verbose opts
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
