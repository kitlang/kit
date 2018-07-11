module Kit.Ir.IrDecl where

  import Kit.Ast
  import Kit.Ir.IrExpr
  import Kit.Parser.Span
  import Kit.Str

  data IrDecl
    = IrVar Str BasicType (Maybe IrExpr)
    | IrFunction Str BasicType IrExpr
    | IrType BasicType
    deriving (Eq, Show)
