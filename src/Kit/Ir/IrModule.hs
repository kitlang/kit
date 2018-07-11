module Kit.Ir.IrModule where

  import Kit.Ast
  import Kit.Ir.IrDecl

  data IrModule = IrModule {
    irmod_path :: ModulePath,
    irmod_declarations :: [IrDecl],
    irmod_includes :: [FilePath]
  }
