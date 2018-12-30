{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module Kit.Ast.Span where

import Data.Hashable
import GHC.Generics
import Control.Applicative

data Span = Span
  { file :: FilePath
  , startLine :: Int
  , startCol :: Int
  , endLine :: Int
  , endCol :: Int
  } deriving (Generic)

pattern NoPos :: Span
pattern NoPos = Span {file = "", startLine = 0, startCol = 0, endLine = 0, endCol = 0}

instance Eq Span where
  (==) NoPos _ = True
  (==) _ NoPos = True
  (==) (a@Span {}) (b@Span {}) = (file a == file b) && (startLine a == startLine b) && (startCol a == startCol b) && (endLine a == endLine b) && (endCol a == endCol b)

instance Show Span where
  show NoPos = "@(???)"
  show span = "@" ++ file span ++
              ":" ++ (show $ startLine span) ++ ":" ++ (show $ startCol span) ++
              (if (startCol span /= endCol span) || (startLine span /= endLine span)
                then "-" ++ (if startLine span /= endLine span then (show $ endLine span) ++ ":" else "") ++ (show $ endCol span)
                else "")

instance Hashable Span

class Positioned a where
  position :: a -> Span

sp :: FilePath -> Int -> Int -> Int -> Int -> Span
sp f a b c d = Span
  { file          = f
  , startLine     = a
  , startCol      = b
  , endLine       = c
  , endCol        = d
  }

(<+>) span1 NoPos = span1
(<+>) NoPos span2 = span2
(<+>) span1 span2 = Span
  { file          = file span1
  , startLine     = fst min
  , startCol      = snd min
  , endLine       = fst max
  , endCol        = snd max
  }
 where
  a1  = (startLine span1, startCol span1)
  a2  = (endLine span1, endCol span1)
  b1  = (startLine span2, startCol span2)
  b2  = (endLine span2, endCol span2)
  min = if a1 < b1 then a1 else b1
  max = if a2 > b2 then a2 else b2
