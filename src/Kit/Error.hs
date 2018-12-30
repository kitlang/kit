{-# LANGUAGE ExistentialQuantification #-}

module Kit.Error where

import Control.Exception
import Control.Monad
import Data.List
import System.Console.ANSI
import System.Directory
import System.IO
import Kit.Log
import Kit.Ast.Span

throwk e = do
  throw $ KitError e

class (Eq a, Show a) => Errable a where
  logError :: a -> IO ()
  errPos :: a -> Maybe Span
  errPos _ = Nothing
  flattenErrors :: a -> [KitError]
  flattenErrors e = [KitError e]

data KitError = forall a . (Show a, Eq a, Errable a) => KitError a
instance Errable KitError where
  logError (KitError e) = logError e
  errPos (KitError e) = errPos e
  flattenErrors (KitError e) = flattenErrors e
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
  flattenErrors (KitErrors e) = nub $ foldr (++) [] (map flattenErrors e)

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
    Just pos -> do
      let f     = file pos
      let start = startLine pos
      hSetSGR
        stderr
        [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
      hPutStr stderr $ "Error: "
      hSetSGR
        stderr
        [SetColor Foreground Vivid White, SetConsoleIntensity BoldIntensity]
      hPutStr stderr $ f ++ ":" ++ (show start) ++ ": "
    _ -> do
      hSetSGR
        stderr
        [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
      hPutStr stderr $ "Error: "
  hSetSGR stderr [Reset]

logErrorBasic :: (Errable e) => e -> String -> IO ()
logErrorBasic err msg = do
  logErrorTitle err
  hPutStrLn stderr msg
  case errPos err of
    Just pos -> displayFileSnippet pos
    _        -> return ()

lpad :: String -> Int -> String
lpad s n = (take (n - length s) (repeat ' ')) ++ s

displayFileSnippet :: Span -> IO ()
displayFileSnippet NoPos = return ()
displayFileSnippet span  = do
  let fp = file span
  exists <- doesFileExist fp
  when exists $ do
    hSetSGR
      stderr
      [SetColor Foreground Vivid Blue, SetConsoleIntensity NormalIntensity]
    hPutStrLn stderr $ "\n  " ++ show span
    contents <- readFile $ fp
    let content_lines = lines contents
    let lineNumbers   = [(startLine span) .. (endLine span)]
    forM_ lineNumbers $ \n -> do
      hSetSGR
        stderr
        [SetColor Foreground Vivid White, SetConsoleIntensity NormalIntensity]
      if n == (startLine span) + 3 && (length lineNumbers > 5)
        then do
          hPutStrLn stderr $ "\n  ...\n"
        else unless (n > (startLine span) + 3 && n < (endLine span) - 2) $ do
          let line = content_lines !! (n - 1)
          hSetSGR stderr [SetConsoleIntensity NormalIntensity]
          hPutStr stderr $ (lpad (show n) 8) ++ "    "
          hSetSGR stderr [SetConsoleIntensity FaintIntensity]
          hPutStrLn stderr $ line
          when
              (  (startLine span)
              <= (endLine span)
              || (startCol span)
              >  1
              || (endCol span)
              <  length line
              )
            $ do
                let this_startCol = if n == startLine span
                      then startCol span
                      else length (takeWhile ((==) ' ') line) + 1
                let this_endCol =
                      if n == endLine span then endCol span else length line
                hSetSGR stderr [Reset]
                ePutStr
                  $  "            "
                  ++ (take ((this_startCol) - 1) (repeat ' '))
                hSetSGR
                  stderr
                  [ SetColor Foreground Vivid Yellow
                  , SetConsoleIntensity BoldIntensity
                  ]
                ePutStrLn
                  (take ((this_endCol) - (this_startCol) + 1) $ repeat '^')
    hSetSGR stderr [Reset]
    case rewrittenFrom span of
      Just x -> do
        hPutStrLn stderr "    ... rewritten from ..."
        displayFileSnippet x
      Nothing -> hPutStrLn stderr ""

forMWithErrors :: [a] -> (a -> IO b) -> IO [b]
forMWithErrors l f = do
  results <- foldM
    (\(errs, results) x -> do
      result <- try $ f x
      case result of
        Left  err -> return (err : errs, results)
        Right r   -> return (errs, r : results)
    )
    ([], [])
    l

  if null $ fst results
    then return $ reverse $ snd results
    else throwk $ KitErrors $ reverse $ fst results

mapMWithErrors f l = forMWithErrors l f

forMWithErrors_ :: [a] -> (a -> IO b) -> IO ()
forMWithErrors_ l f = do
  results <- foldM
    (\errs x -> do
      result <- try $ f x
      case result of
        Left err -> return $ err : errs
        _        -> return errs
    )
    []
    l
  unless (null $ results) $ throwk $ KitErrors $ reverse results

mapMWithErrors_ f l = forMWithErrors_ l f

showErrs e = do
  let errs = flattenErrors e
  mapM_ logError $ errs
  return $ length errs
