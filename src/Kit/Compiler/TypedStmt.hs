module Kit.Compiler.TypedStmt where

import Kit.Ast
import Kit.Compiler.TypedExpr

type TypedStmt = Statement TypedExpr ConcreteType
