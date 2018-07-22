module Kit.Compiler.TypedDecl where

import Kit.Ast
import Kit.Compiler.TypedExpr
import Kit.Str

type TypedVar = VarDefinition TypedExpr ConcreteType
type TypedFunction = FunctionDefinition TypedExpr ConcreteType
type TypedType = TypeDefinition TypedExpr ConcreteType

data TypedDecl
  = TypedVarDecl TypedVar
  | TypedFunctionDecl TypedFunction
  | TypedTypeDecl TypedType
  deriving (Eq, Show)
