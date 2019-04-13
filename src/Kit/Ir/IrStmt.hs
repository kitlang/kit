module Kit.Ir.IrStmt where

import Kit.Ast
import Kit.Ir.IrExpr

type IrStmt = Statement IrExpr BasicType
