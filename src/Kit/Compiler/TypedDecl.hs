module Kit.Compiler.TypedDecl where

import Kit.Ast
import Kit.Compiler.TypedExpr
import Kit.Str

type TypedVar = VarDefinition TypedExpr ConcreteType
type TypedFunction = FunctionDefinition TypedExpr ConcreteType
type TypedType = TypeDefinition TypedExpr ConcreteType

data TypedDecl
  = TypedVar TypedVar
  | TypedFunction TypedFunction
  | TypedType TypedType
  deriving (Eq, Show)
