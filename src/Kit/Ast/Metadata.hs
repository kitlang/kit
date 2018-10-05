module Kit.Ast.Metadata where

import Kit.Ast.Value
import Kit.Str

data Metadata = Metadata {metaName :: Str, metaArgs :: [MetaArg]} deriving (Eq, Show)

data MetaArg
  = MetaIdentifier Str
  | MetaLiteral ValueLiteral
  deriving (Eq, Show)

meta s = Metadata {metaName = s, metaArgs = []}
metaExtern = meta "extern"
metaBuiltin = meta "builtin"

hasMeta :: Str -> [Metadata] -> Bool
hasMeta s [] = False
hasMeta s (h:t) = if metaName h == s then True else hasMeta s t
