module Kit.Parser.Base where

  import Kit.Error

  data Parser a = ParseResult a | Err Error

  instance Functor Parser where
    fmap f (ParseResult x) = ParseResult (f x)
    fmap f (Err e) = Err e

  instance Applicative Parser where
    pure = ParseResult
    (<*>) (ParseResult f) (ParseResult x) = ParseResult (f x)
    (<*>) (Err e) _ = Err e
    (<*>) _ (Err e) = Err e

  instance Monad Parser where
    (>>=) m f = case m of
      ParseResult x -> f x
      Err e -> Err e
    return = ParseResult
