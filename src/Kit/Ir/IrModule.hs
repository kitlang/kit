module Kit.Ir.IrModule where

import Kit.Ast
import Kit.Ir.IrDecl

data IrModule = IrModule {
  irmodPath :: ModulePath,
  irmod_declarations :: [IrDecl],
  irmodIncludes :: [FilePath]
}
