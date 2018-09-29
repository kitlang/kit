module Kit.Compiler.TypedDecl where

import Kit.Ast
import Kit.Compiler.TypedExpr

type TypedDecl = Declaration TypedExpr ConcreteType
