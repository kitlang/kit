module Kit.Parser.Span where

  import Control.Applicative
  import System.FilePath
  import Kit.Str

  data Span = Span {file :: Maybe Str, start_line :: Int, start_col :: Int, end_line :: Int, end_col :: Int} deriving (Eq)

  instance Show Span where
    show span =
      if span == null_span
        then "@(null)"
        else "@" ++ (case file span of {Just f -> s_unpack f; Nothing -> "(nothing)"}) ++ ":" ++ (show $ start_line span) ++ ":" ++ (show $ start_col span) ++ "-" ++ (show $ end_line span) ++ ":" ++ (show $ end_col span)

  null_span :: Span
  null_span = Span {file = Nothing, start_line = 0, start_col = 0, end_line = 0, end_col = 0}

  sp :: Int -> Int -> Int -> Int -> Span
  sp a b c d = Span {file = Nothing, start_line = a, start_col = b, end_line = c, end_col = d}

  fsp :: Str -> Int -> Int -> Int -> Int -> Span
  fsp f a b c d = (sp a b c d) {file = Just f}

  (<+>) span1 span2 =
    if span1 == null_span then span2 else if span2 == null_span then span1 else
    Span {file = (file span1) <|> (file span2), start_line = fst min, start_col = snd min, end_line = fst max, end_col = snd max}
    where
      a1 = (start_line span1, start_col span1)
      a2 = (end_line span1, end_col span1)
      b1 = (start_line span2, start_col span2)
      b2 = (end_line span2, end_col span2)
      min = if a1 < b1 then a1 else b1
      max = if a2 > b2 then a2 else b2
