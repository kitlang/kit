module Kit.Ast.Modifier where

  data Modifier
    = Public
    | Private
    | Macro
    | Inline
    | Override
    deriving (Eq, Show)
