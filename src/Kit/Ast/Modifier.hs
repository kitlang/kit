{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.Modifier where

import GHC.Generics

data Modifier
  = Public
  | Private
  | Inline
  | Static
  deriving (Eq, Generic)

instance Show Modifier where
  show Public = "public"
  show Private = "private"
  show Inline = "inline"
  show Static = "static"

isPublic :: [Modifier] -> Bool
isPublic (Public  : t) = True
isPublic (_       : t) = isPublic t
isPublic []            = False
