module Kit.Ast.Declaration where

import Kit.Ast.Definitions
import Kit.Parser.Span

data Declaration a b
  = DeclVar (VarDefinition a b)
  | DeclFunction (FunctionDefinition a b)
  | DeclType (TypeDefinition a b)
  | DeclTrait (TraitDefinition a b)
  deriving (Eq, Show)
