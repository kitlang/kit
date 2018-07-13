module Kit.Ast.Modifier where

data Modifier
  = Public
  | Private
  | Macro
  | Inline
  | Override
  | Static
  | Const
  deriving (Eq, Show)
