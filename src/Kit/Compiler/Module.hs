module Kit.Compiler.Module where

  import Data.IORef
  import Kit.Ast
  import Kit.Compiler.Scope
  import Kit.Compiler.TypeUsage
  import Kit.HashTable
  import Kit.Ir
  import Kit.Parser.Span
  import Kit.Str

  data Module = Module {
    mod_path :: ModulePath,
    mod_contents :: IORef [Statement],
    mod_imports :: [(ModulePath, Span)],
    mod_includes :: [(FilePath, Span)],
    mod_types :: Scope TypeUsage,
    mod_functions :: Scope FunctionDefinition,
    mod_vars :: Scope Binding,
    mod_enums :: Scope EnumConstructor,
    mod_ir :: IORef [IrDecl]
  }

  newMod :: ModulePath -> [Statement] -> IO Module
  newMod path stmts = do
    types <- newScope
    vars <- newScope
    contents <- newIORef stmts
    ir <- newIORef []
    functions <- newScope
    return $ Module {
        mod_path = path,
        mod_contents = contents,
        mod_imports = _findImports path stmts,
        mod_includes = _findIncludes stmts,
        mod_types = types,
        mod_functions = functions,
        mod_vars = vars,
        mod_ir = ir
      }

  newCMod :: IO Module
  newCMod = do
    types <- newScope
    vars <- newScope
    enums <- newScope
    contents <- newIORef []
    ir <- newIORef []
    functions <- newScope
    return $ Module {
        mod_path = [],
        mod_contents = contents,
        mod_imports = [],
        mod_includes = [],
        mod_types = types,
        mod_functions = functions,
        mod_vars = vars,
        mod_enums = enums,
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

  findEnumType :: Module -> Str -> IO (Maybe EnumConstructor)
  findEnumType mod s = resolveLocal (mod_enums mod) s
