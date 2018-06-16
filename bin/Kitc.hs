{-# LANGUAGE OverloadedStrings #-}

module Main where

  import System.Environment
  import Options.Applicative
  import Data.Semigroup ((<>))
  import Kit

  data Options = Options {
    show_version :: Bool,
    verbose :: Bool,
    target :: String,
    main_class :: String,
    output_dir :: String,
    source_paths :: [String],
    defines :: [String]
  } deriving (Eq, Show)

  options :: Parser Options
  options = Options
    <$> switch (long "version" <> short 'v' <> help "show the version number and exit")
    <*> switch (long "verbose" <> help "show extra debugging output")
    <*> strOption (long "target" <> short 't' <> showDefault <> value "c" <> metavar "TARGET" <> help "compile target (c|web|eval)")
    <*> strOption (long "main" <> short 'm' <> showDefault <> value "main" <> metavar "MODULE" <> help "module path containing main() function")
    <*> strOption (long "output" <> short 'o' <> showDefault <> value "build" <> metavar "DIR" <> help "sets the output directory")
    <*> many sourceDirParser
    <*> many definesParser

  sourceDirParser :: Parser String
  sourceDirParser = strOption (long "src" <> short 's' <> metavar "DIR" <> help "add a source directory")

  definesParser :: Parser String
  definesParser = strOption (long "define" <> short 'D' <> metavar "KEY[=VAL]" <> help "add a define")

  helper' :: Parser (a -> a)
  helper' = abortOption ShowHelpText $ mconcat [long "help", short 'h', help "show this help text and exit", hidden]

  main :: IO ()
  main = do
    argv <- getArgs
    let args = if length argv == 0 then ["--help"] else argv

    opts <- handleParseResult $ execParserPure p (info (options <**> helper') (fullDesc
                <> progDesc "This is the Kit compiler. It is generally used via the 'kit' build tool."
                <> header ("kitc v" ++ version)
              )) args

    if show_version opts
      then putStrLn $ "kitc v" ++ version
      else do return ()
    where
      p = prefs (showHelpOnError)
