module Kit.Ir.IrDecl where

import Kit.Ast
import Kit.Ir.IrExpr
import Kit.Parser.Span
import Kit.Str

type IrVar = VarDefinition IrExpr BasicType
type IrFunction = FunctionDefinition IrExpr BasicType
type IrType = TypeDefinition IrExpr BasicType

data IrDecl
  = IrVar IrVar
  | IrFunction IrFunction
  | IrType IrType
  deriving (Eq, Show)
