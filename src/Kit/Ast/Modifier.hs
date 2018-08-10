module Kit.Ast.Modifier where

data Modifier
  = Public
  | Private
  | Inline
  | Const
  | Static
  deriving (Eq)

instance Show Modifier where
  show Public = "public"
  show Private = "private"
  show Inline = "inline"
  show Const = "const"
  show Static = "static"
