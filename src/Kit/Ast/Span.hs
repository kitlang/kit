module Kit.Ast.Span where

  data Span = Span {start_line :: Int, start_col :: Int, end_line :: Int, end_col :: Int} deriving (Eq, Show)

  null_span = Span {start_line = 0, start_col = 0, end_line = 0, end_col = 0}
