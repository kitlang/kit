{-# LANGUAGE DeriveAnyClass #-}

module Kit.Error where

  import Control.Exception
  import Control.Monad
  import System.Console.ANSI
  import System.IO
  import Kit.Parser.Span

  data ErrorType
    = ParseError
    | ImportError
    | Unknown
    deriving (Eq, Show)

  data Error = Error {
    err_msg :: String,
    err_pos :: Maybe Span,
    err_file :: Maybe FilePath,
    err_type :: ErrorType
    } deriving (Eq, Show, Exception)

  newtype Errors = Errs [Error] deriving (Eq, Show, Exception)

  err :: ErrorType -> String -> Error
  err t msg = Error { err_msg = msg, err_pos = Nothing, err_file = Nothing, err_type = t }
  errp :: ErrorType -> String -> Span -> Error
  errp t msg p = Error { err_msg = msg, err_pos = Just p, err_file = Nothing, err_type = t }

  errf :: Error -> FilePath -> Error
  errf e fp = e {err_file = Just fp}

  logError :: Error -> IO ()
  logError e = do
    case (err_file e, err_pos e) of
      (Just f, Just s) -> do
        setSGR [SetColor Foreground Vivid White]
        hPutStr stderr $ f ++ ":" ++ (show $ start_line s) ++ ": "
        setSGR [SetColor Foreground Dull White]
        hPutStrLn stderr $ err_msg e
        displayFileSnippet f s
        setSGR [Reset]
      _ -> do
        setSGR [SetColor Foreground Vivid White]
        hPutStr stderr $ "Error: "
        setSGR [SetColor Foreground Dull White]
        hPutStrLn stderr $ err_msg e
        setSGR [Reset]

  lpad :: String -> Int -> String
  lpad s n = (take (n - length s) (repeat ' ')) ++ s

  displayFileSnippet :: FilePath -> Span -> IO ()
  displayFileSnippet fp span = do
    setSGR [SetColor Foreground Vivid White]
    hPutStrLn stderr $ "\n  " ++ (show fp) ++ ":"
    contents <- readFile $ fp
    let content_lines = lines contents
    forM_ [(end_line span) .. (start_line span)] $ \n -> do
      if n == (start_line span) + 2
        then do hPutStrLn stderr $ "\n  ...\n"
        else if n > (start_line span) + 2 && n < (end_line span) - 1
          then do return ()
          else do
            let line = content_lines !! (n - 1)
            setSGR [SetColor Foreground Vivid White]
            hPutStr stderr $ (lpad (show n) 8) ++ "    "
            setSGR [SetColor Foreground Dull White]
            hPutStrLn stderr $ line
            if (start_line span) < (end_line span) || (start_col span) > 1 || (end_col span) < length line
              then do
                setSGR [SetColor Foreground Vivid Yellow]
                hPutStrLn stderr $ "            " ++ (take ((start_col span) - 1) (repeat ' ')) ++ (take ((end_col span) - (start_col span) + 1) $ repeat '^')
              else do return ()
    return ()
