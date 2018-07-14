module Kit.Ast.Metadata where

import Kit.Ast.Lvalue
import Kit.Ast.Value
import Kit.Str

data Metadata = Metadata {metaName :: Str, metaArgs :: [MetaArg]} deriving (Eq, Show)

data MetaArg
  = MetaLvalue Lvalue
  | MetaLiteral ValueLiteral
  deriving (Eq, Show)

meta s = Metadata {metaName = s, metaArgs = []}
metaExtern = meta "extern"
