module Kit.Ast.Identifier where

import Kit.Ast.TypeSpec
import Kit.Str

data Identifier
  -- A variable name
  = Var Str
  -- A macro variable with optional type annotation:
  -- `$abc` or `${abc: Int}`
  | MacroVar Str (Maybe TypeSpec)
  deriving (Eq, Show)

identifierName x = case x of
  Var s        -> s
  MacroVar s _ -> s
