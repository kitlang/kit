module Kit.Compiler.Generators.StringCompare where

import Kit.Ast
import Kit.Ir
import Kit.Str

stringCompare :: IrExpr -> Str -> IrExpr
stringCompare ex s = IrBinop
  Eq
  (IrCall
    (IrIdentifier "strncmp")
    [ ex
    , IrLiteral $ StringValue s
    , IrLiteral $ IntValue (fromIntegral $ s_length s) BasicTypeTrueInt
    ]
  )
  (IrLiteral $ IntValue 0 BasicTypeTrueInt)
