module Kit.Ir.IrModule where

import Kit.Ast
import Kit.Ir.IrStmt

data IrModule = IrModule {
  irmodPath :: ModulePath,
  irmod_declarations :: [IrStmt],
  irmodIncludes :: [FilePath]
}
