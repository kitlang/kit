{-# LANGUAGE DeriveAnyClass #-}

module Kit.Error where

  import Control.Exception
  import Control.Monad
  import System.Console.ANSI
  import System.IO
  import Kit.Parser.Span
  import Kit.Str

  data ErrorType
    = ParseError
    | ImportError
    | IncludeError
    | ValidationError
    | TypingError
    | UnificationError
    | InternalError
    | Unknown
    deriving (Eq, Show)

  data Error = Error {
    err_msg :: String,
    err_pos :: Maybe Span,
    err_type :: ErrorType
    } deriving (Eq, Show, Exception)

  newtype Errors = Errs [Error] deriving (Eq, Show, Exception)

  err :: ErrorType -> String -> Error
  err t msg = Error { err_msg = msg, err_pos = Nothing, err_type = t }
  errp :: ErrorType -> String -> Maybe Span -> Error
  errp t msg p = Error { err_msg = msg, err_pos = p, err_type = t }

  logError :: Error -> IO ()
  logError e = do
    hSetSGR stderr [SetColor Foreground Vivid White, SetConsoleIntensity NormalIntensity]
    hPutStrLn stderr $ take 40 (repeat '-')
    case err_pos e of
      Just pos@Span {file = Just f, start_line = start} -> do
        hSetSGR stderr [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
        hPutStr stderr $ "Error: "
        hSetSGR stderr [SetColor Foreground Vivid White, SetConsoleIntensity BoldIntensity]
        hPutStr stderr $ (s_unpack f) ++ ":" ++ (show start) ++ ": "
        hSetSGR stderr [SetColor Foreground Vivid White, SetConsoleIntensity NormalIntensity]
        hPutStrLn stderr $ err_msg e
        displayFileSnippet (s_unpack f) pos
      _ -> do
        hSetSGR stderr [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
        hPutStr stderr $ "Error: "
        hSetSGR stderr [SetColor Foreground Vivid White, SetConsoleIntensity NormalIntensity]
        hPutStrLn stderr $ err_msg e
    hSetSGR stderr [Reset]

  lpad :: String -> Int -> String
  lpad s n = (take (n - length s) (repeat ' ')) ++ s

  displayFileSnippet :: FilePath -> Span -> IO ()
  displayFileSnippet fp span = do
    hSetSGR stderr [SetColor Foreground Vivid Blue, SetConsoleIntensity NormalIntensity]
    hPutStrLn stderr $ "\n  <" ++ fp ++ ">:"
    contents <- readFile $ fp
    let content_lines = lines contents
    putStrLn $ show span
    forM_ [(start_line span) .. (end_line span)] $ \n -> do
      hSetSGR stderr [SetColor Foreground Vivid White, SetConsoleIntensity NormalIntensity]
      if n == (start_line span) + 3
        then do hPutStrLn stderr $ "\n  ...\n"
        else if n > (start_line span) + 3 && n < (end_line span) - 2
          then do return ()
          else do
            let line = content_lines !! (n - 1)
            hSetSGR stderr [SetConsoleIntensity NormalIntensity]
            hPutStr stderr $ (lpad (show n) 8) ++ "    "
            hSetSGR stderr [SetConsoleIntensity FaintIntensity]
            hPutStrLn stderr $ line
            if (start_line span) <= (end_line span) || (start_col span) > 1 || (end_col span) < length line
              then do
                hSetSGR stderr [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity]
                let this_start_col = if n == start_line span then start_col span else length (takeWhile ((==) ' ') line) + 1
                let this_end_col = if n == end_line span then end_col span else length line
                hPutStrLn stderr $ "            " ++ (take ((this_start_col) - 1) (repeat ' ')) ++ (take ((this_end_col) - (this_start_col) + 1) $ repeat '^')
              else do return ()
    hPutStrLn stderr ""
    return ()
