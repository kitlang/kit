module Kit.Ast.Identifier where

import Kit.Ast.TypeSpec
import Kit.Str

data Identifier
  -- A variable name
  = Var Str
  -- A macro variable with optional type annotation:
  -- `$abc` or `${abc: Int}`
  | MacroVar Str (Maybe TypeSpec)
  deriving (Eq)

instance Show Identifier where
  show (Var s) = s_unpack s
  show (MacroVar s (Just x)) = "${" ++ s_unpack s ++ ": " ++ show x ++ "}"
  show (MacroVar s Nothing) = "$" ++ s_unpack s

identifierName x = case x of
  Var s        -> s
  MacroVar s _ -> s
