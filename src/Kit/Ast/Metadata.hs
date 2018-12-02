{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.Metadata where

import GHC.Generics
import Kit.Ast.Value
import Kit.Str

data Metadata = Metadata {metaName :: Str, metaArgs :: [MetaArg]} deriving (Eq, Generic, Show)

data MetaArg
  = MetaIdentifier Str
  | MetaLiteral ValueLiteral
  deriving (Eq, Generic, Show)

meta s = Metadata {metaName = s, metaArgs = []}
metaExtern = "extern" :: Str
metaBuiltin = "builtin" :: Str
-- "promote" allows base types to be used as their abstract type
metaPromote = "promote" :: Str
-- "demote" allows abstract types to unify strictly with their base type
metaDemote = "demote" :: Str
metaNoreturn = "noreturn" :: Str
metaConst = "const" :: Str

hasMeta :: Str -> [Metadata] -> Bool
hasMeta s []      = False
hasMeta s (h : t) = if metaName h == s then True else hasMeta s t
