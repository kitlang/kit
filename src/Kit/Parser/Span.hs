module Kit.Parser.Span where

import Control.Applicative
import System.FilePath
import Kit.Str

data Span
  = Span {file :: FilePath, start_line :: Int, start_col :: Int, end_line :: Int, end_col :: Int, rewritten :: Maybe Span}
  | NoPos

instance Eq Span where
  (==) NoPos _ = True
  (==) _ NoPos = True
  (==) a b = (file a == file b) && (start_line a == start_line b) && (start_col a == start_col b) && (end_line a == end_line b) && (end_col a == end_col b)

instance Show Span where
  show NoPos = "@(???)"
  show span = "@" ++ file span ++
              ":" ++ (show $ start_line span) ++ ":" ++ (show $ start_col span) ++
              (if (start_col span /= end_col span) || (start_line span /= end_line span)
                then "-" ++ (show $ end_line span) ++ ":" ++ (show $ end_col span)
                else "")

sp :: FilePath -> Int -> Int -> Int -> Int -> Span
sp f a b c d = Span
  { file       = f
  , start_line = a
  , start_col  = b
  , end_line   = c
  , end_col    = d
  , rewritten  = Nothing
  }

(<+>) span1 NoPos = span1
(<+>) NoPos span2 = span2
(<+>) span1 span2 = Span
  { file       = file span1
  , start_line = fst min
  , start_col  = snd min
  , end_line   = fst max
  , end_col    = snd max
  }
 where
  a1  = (start_line span1, start_col span1)
  a2  = (end_line span1, end_col span1)
  b1  = (start_line span2, start_col span2)
  b2  = (end_line span2, end_col span2)
  min = if a1 < b1 then a1 else b1
  max = if a2 > b2 then a2 else b2
