module Kit.Ir.IrExpr where

import Kit.Str
import Kit.Ast
import Kit.Parser.Span

data IrExpr
  = IrBlock [IrExpr]
  | IrLiteral (ValueLiteral BasicType)
  | IrIdentifier Str
  | IrPreUnop Operator IrExpr
  | IrPostUnop Operator IrExpr
  | IrBinop Operator IrExpr IrExpr
  | IrFor Str BasicType IrExpr IrExpr IrExpr
  | IrWhile IrExpr IrExpr Bool
  | IrIf IrExpr IrExpr (Maybe IrExpr)
  | IrContinue
  | IrBreak
  | IrReturn (Maybe IrExpr)
  | IrField IrExpr Str
  | IrArrayAccess IrExpr IrExpr
  | IrCall IrExpr [IrExpr]
  | IrCast IrExpr BasicType
  | IrCArrLiteral [IrExpr]
  | IrVarDeclaration Str BasicType (Maybe IrExpr)
  | IrStructInit BasicType [(Str, IrExpr)]
  | IrEnumInit BasicType Str [IrExpr]
  | IrTupleInit BasicType [IrExpr]
  deriving (Eq, Show)
