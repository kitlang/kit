module Kit.Error where

  import System.Console.ANSI
  import System.IO
  import Kit.Parser.Span

  data ErrorType
    = ParseError
    | ImportError
    deriving (Eq, Show)

  data Error = Error {
    err_msg :: String,
    err_pos :: Maybe FileSpan,
    err_type :: ErrorType
    } deriving (Eq, Show)

  errp t msg p = Error { err_msg = msg, err_pos = Just p, err_type = t }
  err t msg = Error { err_msg = msg, err_pos = Nothing, err_type = t }

  logError :: Error -> IO ()
  logError e = do
    case err_pos e of
      Just (f,s) -> do
        setSGR [SetColor Foreground Vivid White]
        hPutStr stderr $ f ++ ":" ++ (show $ start_line s) ++ ": "
        setSGR [Reset]
        hPutStrLn stderr $ err_msg e
        displayFileSnippet (f,s)
      Nothing -> do
        setSGR [SetColor Foreground Vivid White]
        hPutStr stderr $ "Error: "
        setSGR [Reset]
        hPutStrLn stderr $ err_msg e

  displayFileSnippet :: FileSpan -> IO ()
  displayFileSnippet fs = do
    hPutStrLn stderr "(file would go here)"
