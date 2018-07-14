module Kit.Ast.Lvalue where

import Kit.Ast.TypeSpec
import Kit.Str

data Lvalue
  = Var Str
  | MacroVar Str (Maybe TypeSpec)
  deriving (Eq, Show)

lvalueName x = case x of
  Var s        -> s
  MacroVar s _ -> s
