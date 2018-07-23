module Kit.Ir.IrDecl where

import Kit.Ast
import Kit.Ir.IrExpr
import Kit.Parser.Span
import Kit.Str

type IrDecl = Declaration IrExpr BasicType
