module Kit.Ast.Lvalue where

import Kit.Str

data Lvalue
  = Var Str
  | MacroVar Str
  deriving (Eq, Show)

lvalueName x = case x of
  Var      s -> s
  MacroVar s -> s
