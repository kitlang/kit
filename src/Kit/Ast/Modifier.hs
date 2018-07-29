module Kit.Ast.Modifier where

data Modifier
  = Public
  | Private
  | Inline
  | Const
  | Static
  deriving (Eq, Show)

