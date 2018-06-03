module Kit.Ast.Operator where

  import qualified Data.ByteString.Lazy.Char8 as B

  data Operator
    = Inc
    | Dec
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Eq
    | Neq
    | Gte
    | Lte
    | LeftShift
    | RightShift
    | Gt
    | Lt
    | And
    | Or
    | BitAnd
    | BitOr
    | BitXor
    | Invert
    | InvertBits
    | Cons
    | Ref
    | Deref
    | Assign
    | AssignOp Operator
    | Custom B.ByteString
    deriving (Eq, Show)
