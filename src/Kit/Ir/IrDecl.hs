module Kit.Ir.IrDecl where

import Kit.Ast
import Kit.Ir.IrExpr

type IrDecl = Declaration IrExpr BasicType
