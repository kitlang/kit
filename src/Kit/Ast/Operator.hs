{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.Operator where

import Data.Hashable
import GHC.Generics
import Kit.Str

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
  | Custom Str
  deriving (Eq, Generic)

instance Hashable Operator

instance Show Operator where
  show op = case op of
    Inc -> "++"
    Dec -> "--"
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Mod -> "%"
    Eq -> "=="
    Neq -> "!="
    Gte -> ">="
    Lte -> "<="
    LeftShift -> "<<<"
    RightShift -> ">>>"
    Gt -> ">"
    Lt -> "<"
    And -> "&&"
    Or -> "||"
    BitAnd -> "&"
    BitOr -> "|"
    BitXor -> "^"
    Invert -> "!"
    InvertBits -> "~"
    Cons -> "::"
    Ref -> "&"
    Deref -> "*"
    Assign -> "="
    AssignOp op -> "=" ++ (show op)
    Custom op -> "custom operator"
