module Kit.Parser.Span where

  import System.FilePath

  data Span = Span {start_line :: Int, start_col :: Int, end_line :: Int, end_col :: Int} deriving (Eq, Show)
  type FileSpan = (FilePath, Span)

  null_span :: Span
  null_span = Span {start_line = 0, start_col = 0, end_line = 0, end_col = 0}

  sp :: Int -> Int -> Int -> Int -> Span
  sp a b c d = Span {start_line = a, start_col = b, end_line = c, end_col = d}

  fsp :: FilePath -> Int -> Int -> Int -> Int -> FileSpan
  fsp f a b c d = (f, sp a b c d)

  (<+>) span1 span2 =
    if span1 == null_span then span2 else if span2 == null_span then span1 else
    Span {start_line = fst min, start_col = snd min, end_line = fst max, end_col = snd max}
    where
      a1 = (start_line span1, start_col span1)
      a2 = (end_line span1, end_col span1)
      b1 = (start_line span2, start_col span2)
      b2 = (end_line span2, end_col span2)
      min = if a1 < b1 then a1 else b1
      max = if a2 > b2 then a2 else b2
