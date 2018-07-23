module Kit.Compiler.TypedDecl where

import Kit.Ast
import Kit.Compiler.TypedExpr
import Kit.Str

type TypedDecl = Declaration TypedExpr ConcreteType
