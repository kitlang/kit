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
    mod_contents :: IORef [Expr],
    mod_imports :: [(ModulePath, Span)],
    mod_includes :: [(FilePath, Span)],
    mod_types :: Scope TypeUsage,
    mod_vars :: Scope Binding,
    mod_ir :: IORef [IrDecl]
  }

  newMod :: ModulePath -> [Expr] -> IO Module
  newMod path exprs = do
    types <- newScope
    vars <- newScope
    contents <- newIORef exprs
    ir <- newIORef []
    return $ Module {
        mod_path = path,
        mod_contents = contents,
        mod_imports = _findImports path exprs,
        mod_includes = _findIncludes exprs,
        mod_types = types,
        mod_vars = vars,
        mod_ir = ir
      }

  newCMod :: [Expr] -> IO Module
  newCMod exprs = do
    types <- newScope
    vars <- newScope
    contents <- newIORef exprs
    ir <- newIORef []
    return $ Module {
        mod_path = [],
        mod_contents = contents,
        mod_imports = [],
        mod_includes = [],
        mod_types = types,
        mod_vars = vars,
        mod_ir = ir
      }

  _findImports :: ModulePath -> [Expr] -> [(ModulePath, Span)]
  _findImports mod exprs = foldr (\e acc -> case e of
      Expr {expr = Import mp, pos = p} ->
        -- eliminate self imports (e.g. from prelude)
        if mod == mp
          then acc
          else (mp, p) : acc
      _ -> acc
    ) [] exprs

  _findIncludes :: [Expr] -> [(FilePath, Span)]
  _findIncludes exprs = foldr (\e acc -> case e of
      Expr {expr = Include ip, pos = p} -> (ip, p) : acc
      _ -> acc
    ) [] exprs

  getConcreteType :: ModulePath -> TypeDefinition -> ConcreteType
  getConcreteType mod t@(TypeDefinition {type_name = s, type_type = Atom}) = TypeAtom s
  getConcreteType mod t@(TypeDefinition {type_name = s, type_type = Struct {}}) = TypeStruct mod s
  getConcreteType mod t@(TypeDefinition {type_name = s, type_type = Enum {}}) = TypeEnum mod s
  getConcreteType mod t@(TypeDefinition {type_name = s, type_type = Abstract {}}) = TypeAbstract mod s
  getConcreteType mod t@(TypeDefinition {type_name = s, type_type = Typedef {}}) = TypeTypedef mod s

  findModuleType :: Module -> TypeSpec -> IO (Maybe ConcreteType)
  findModuleType mod (TypeSpec (path, typeName) params) = do
    if path == [] || path == (mod_path mod)
      then do
        usage <- resolveLocal (mod_types mod) typeName
        case usage of
          Just (u@TypeUsage {type_definition = t}) -> return $ Just $ getConcreteType (mod_path mod) t
          Nothing -> return $ Nothing
      else return Nothing
