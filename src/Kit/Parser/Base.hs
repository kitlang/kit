module Kit.Parser.Base where

import Kit.Error
import Kit.Ast.Span

data Parser a = ParseResult a | Err KitError

data ParseError = ParseError String (Maybe Span) deriving (Eq, Show)
instance Errable ParseError where
  logError reader err@(ParseError msg _) = logErrorBasic reader (KitError err) msg
  errPos (ParseError _ pos) = pos

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
