module Kit.Compiler.Module where

  import Data.IORef
  import Kit.Ast
  import Kit.Compiler.Scope
  import Kit.Compiler.TypedDecl
  import Kit.Compiler.TypedExpr
  import Kit.Compiler.TypeUsage
  import Kit.HashTable
  import Kit.Ir
  import Kit.Parser.Span
  import Kit.Str

  data Module = Module {
    mod_path :: ModulePath,
    mod_source_path :: FilePath,
    mod_contents :: IORef [Statement],
    mod_imports :: [(ModulePath, Span)],
    mod_includes :: [(FilePath, Span)],
    mod_types :: Scope ConcreteType,
    mod_functions :: Scope FunctionDefinition,
    mod_type_definitions :: Scope TypeUsage,
    mod_vars :: Scope Binding,
    mod_typed_contents :: Scope TypedDecl,
    mod_ir :: IORef [IrDecl]
  }

  instance Show Module where
    show m = "<module " ++ (s_unpack $ showModulePath $ mod_path m) ++ " (" ++ (mod_source_path m) ++ ")>"

  newMod :: ModulePath -> [Statement] -> FilePath -> IO Module
  newMod path stmts fp = do
    contents <- newIORef stmts
    types <- newScope
    functions <- newScope
    typeDefinitions <- newScope
    vars <- newScope
    enums <- newScope
    typedContents <- newScope
    ir <- newIORef []
    return $ Module {
        mod_path = path,
        mod_source_path = fp,
        mod_contents = contents,
        mod_imports = _findImports path stmts,
        mod_includes = _findIncludes stmts,
        mod_types = types,
        mod_functions = functions,
        mod_type_definitions = typeDefinitions,
        mod_vars = vars,
        mod_typed_contents = typedContents,
        mod_ir = ir
      }

  newCMod :: FilePath -> IO Module
  newCMod fp = do
    contents <- newIORef []
    types <- newScope
    functions <- newScope
    typeDefinitions <- newScope
    vars <- newScope
    enums <- newScope
    typedContents <- newScope
    ir <- newIORef []
    return $ Module {
        mod_path = [],
        mod_source_path = fp,
        mod_contents = contents,
        mod_imports = [],
        mod_includes = [],
        mod_types = types,
        mod_functions = functions,
        mod_type_definitions = typeDefinitions,
        mod_vars = vars,
        mod_typed_contents = typedContents,
        mod_ir = ir
      }

  _findImports :: ModulePath -> [Statement] -> [(ModulePath, Span)]
  _findImports mod stmts = foldr (\e acc -> case e of
      Statement {stmt = Import mp, stmtPos = p} ->
        -- eliminate self imports (e.g. from prelude)
        if mod == mp
          then acc
          else (mp, p) : acc
      _ -> acc
    ) [] stmts

  _findIncludes :: [Statement] -> [(FilePath, Span)]
  _findIncludes stmts = foldr (\e acc -> case e of
      Statement {stmt = Include ip, stmtPos = p} -> (ip, p) : acc
      _ -> acc
    ) [] stmts
