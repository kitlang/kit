{-
  This package is used during Kit.Compiler.Passes.GenerateIr to generate IR and
  BasicTypes from typed expressions and ConcreteTypes.
-}
module Kit.Compiler.Ir (
  module Kit.Compiler.Ir.FindUnderlyingType,
  module Kit.Compiler.Ir.DeclarationToIr,
  module Kit.Compiler.Ir.ExprToIr,
  module Kit.Compiler.Ir.PatternMatchToIr,
  module Kit.Compiler.Ir.StringCompare
) where

import Kit.Compiler.Ir.FindUnderlyingType
import Kit.Compiler.Ir.DeclarationToIr
import Kit.Compiler.Ir.ExprToIr
import Kit.Compiler.Ir.PatternMatchToIr
import Kit.Compiler.Ir.StringCompare
