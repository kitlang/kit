module Kit.Ast.TypedStmt where

import Kit.Ast.ConcreteType
import Kit.Ast.Statement
import Kit.Ast.TypedExpr

type TypedStmt = Statement TypedExpr ConcreteType
