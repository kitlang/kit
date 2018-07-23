module Kit.Ast.Declaration where

import Kit.Ast.Definitions

data Declaration a b
  = DeclVar (VarDefinition a b)
  | DeclFunction (FunctionDefinition a b)
  | DeclType (TypeDefinition a b)
