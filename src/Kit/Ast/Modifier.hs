module Kit.Ast.Modifier where

data Modifier
  = Public
  | Private
  | Inline
  | Static
  deriving (Eq)

instance Show Modifier where
  show Public = "public"
  show Private = "private"
  show Inline = "inline"
  show Static = "static"
