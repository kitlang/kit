{-# LANGUAGE ExistentialQuantification #-}

module Kit.Error where

import Control.Applicative
import Control.Exception
import Control.Monad
import System.Console.ANSI
import System.IO
import Kit.Parser.Span
import Kit.Str

ePutStr = hPutStr stderr
ePutStrLn = hPutStrLn stderr

throwk e = do throw $ KitError e

class (Eq a, Show a) => Errable a where
  logError :: a -> IO ()
  errPos :: a -> Maybe Span
  errPos _ = Nothing

data KitError = forall a . (Show a, Eq a, Errable a) => KitError a
instance Errable KitError where
  logError (KitError e) = logError e
  errPos (KitError e) = errPos e
instance Show KitError where
  show (KitError e) = show e
instance Eq KitError where
  (==) (KitError x) (KitError y) = (show x) == (show y)

instance Exception KitError

{-
  Used to throw multiple errors simultaneously.
-}
data Errors = KitErrors [KitError] deriving (Eq, Show)
instance Errable Errors where
  logError (KitErrors e) = forM_ e logError
  errPos (KitErrors e) = msum (map errPos e)

{-
  Reperesents a compiler assertion failure; should always be reported.
-}
data InternalError = InternalError String (Maybe Span) deriving (Eq, Show)
instance Errable InternalError where
  logError e@(InternalError s _) = logErrorBasic (KitError e) s
  errPos (InternalError _ pos) = pos

{-
  A simple error type with message and optional position.
-}
data BasicError = BasicError String (Maybe Span) deriving (Eq, Show)
instance Errable BasicError where
  logError e@(BasicError s _) = logErrorBasic (KitError e) s
  errPos (BasicError _ pos) = pos

-- data Error
--   = ParseError Str Span
--   | ImportError ModulePath [FilePath] Span
--   | CIncludeError FilePath [FilePath]
--   | CParseError FilePath Str
--   | ValidationError Str
--   | CodeGenError Str Span
--   | TypingError Str Span
--   | UnificationError TypeConstraint Span
--   | InternalError Str
--   | Errors [Error]
--   deriving (Eq, Show)

-- errPos (ParseError _ pos) = Just pos
-- errPos (ImportError _ _ pos) = Just pos
-- errPos (CodeGenError _ pos) = Just pos
-- errPos (TypingError _ pos) = Just pos
-- errPos (UnificationError _ pos) = Just pos
-- errPos _ = Nothing

logErrorTitle :: (Errable e) => e -> IO ()
logErrorTitle err = do
  hSetSGR
    stderr
    [SetColor Foreground Vivid White, SetConsoleIntensity NormalIntensity]
  hPutStrLn stderr $ take 40 (repeat '-')
  case errPos err of
    Just pos@Span { file = Just f, start_line = start } -> do
      hSetSGR
        stderr
        [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
      hPutStr stderr $ "Error: "
      hSetSGR
        stderr
        [SetColor Foreground Vivid White, SetConsoleIntensity BoldIntensity]
      hPutStr stderr $ (s_unpack f) ++ ":" ++ (show start) ++ ": "
    _ -> do
      hSetSGR
        stderr
        [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
      hPutStr stderr $ "Error: "
  hSetSGR
    stderr
    [SetColor Foreground Vivid White, SetConsoleIntensity NormalIntensity]

logErrorBasic :: (Errable e) => e -> String -> IO ()
logErrorBasic err msg = do
  logErrorTitle err
  hSetSGR stderr [Reset]
  hPutStrLn stderr msg
  case errPos err of
    Just pos@Span { file = Just f } -> displayFileSnippet (s_unpack f) pos
    _                               -> return ()

lpad :: String -> Int -> String
lpad s n = (take (n - length s) (repeat ' ')) ++ s

displayFileSnippet :: FilePath -> Span -> IO ()
displayFileSnippet fp span = do
  hSetSGR
    stderr
    [SetColor Foreground Vivid Blue, SetConsoleIntensity NormalIntensity]
  if span == null_span
    then hPutStrLn stderr $ "\n  " ++ show fp
    else hPutStrLn stderr $ "\n  " ++ show span
  contents <- readFile $ fp
  let content_lines = lines contents
  forM_ [(start_line span) .. (end_line span)] $ \n -> do
    hSetSGR
      stderr
      [SetColor Foreground Vivid White, SetConsoleIntensity NormalIntensity]
    if n == (start_line span) + 3
      then do
        hPutStrLn stderr $ "\n  ...\n"
      else if n > (start_line span) + 3 && n < (end_line span) - 2
        then do
          return ()
        else do
          let line = content_lines !! (n - 1)
          hSetSGR stderr [SetConsoleIntensity NormalIntensity]
          hPutStr stderr $ (lpad (show n) 8) ++ "    "
          hSetSGR stderr [SetConsoleIntensity FaintIntensity]
          hPutStrLn stderr $ line
          if (start_line span)
             <= (end_line span)
             || (start_col span)
             >  1
             || (end_col span)
             <  length line
          then
            do
              hSetSGR
                stderr
                [ SetColor Foreground Vivid Yellow
                , SetConsoleIntensity BoldIntensity
                ]
              let this_start_col = if n == start_line span
                    then start_col span
                    else length (takeWhile ((==) ' ') line) + 1
              let this_end_col =
                    if n == end_line span then end_col span else length line
              hPutStrLn stderr
                $  "            "
                ++ (take ((this_start_col) - 1) (repeat ' '))
                ++ (take ((this_end_col) - (this_start_col) + 1) $ repeat '^')
          else
            do
              return ()
  hPutStrLn stderr ""
  return ()
