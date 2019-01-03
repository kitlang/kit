module Kit.Compiler.Ir.StringCompare where

import Kit.Ast
import Kit.Ir
import Kit.Str

stringCompare :: IrExpr -> Str -> IrExpr
stringCompare ex s = IrBinop
  Eq
  (IrCall
    (IrIdentifier ([], "strcmp"))
    [ ex
    , IrLiteral (StringValue s) (CPtr BasicTypeCChar)
    ]
  )
  (IrLiteral (IntValue 0) BasicTypeCInt)
