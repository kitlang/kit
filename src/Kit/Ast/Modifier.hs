module Kit.Ast.Modifier where

  data Modifier
    = Public
    | Private
    | Macro
    | Inline
    | Override
    | Static
    deriving (Eq, Show)
