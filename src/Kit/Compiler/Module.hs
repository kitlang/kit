module Kit.Compiler.Module where

  import Data.IORef
  import Kit.Ast
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
    mod_types :: HashTable Str TypeUsage,
    mod_vars :: HashTable Str Binding,
    mod_ir :: IORef [IrDecl]
  }

  newMod :: ModulePath -> [Expr] -> IO Module
  newMod path exprs = do
    types <- h_new
    vars <- h_new
    contents <- newIORef exprs
    return $ Module {
        mod_path = path,
        mod_contents = contents,
        mod_imports = findImports path exprs,
        mod_includes = findIncludes exprs,
        mod_types = types,
        mod_vars = vars
      }

  newCMod :: [Expr] -> IO Module
  newCMod exprs = do
    types <- h_new
    vars <- h_new
    contents <- newIORef exprs
    return $ Module {
        mod_path = [],
        mod_contents = contents,
        mod_imports = [],
        mod_includes = [],
        mod_types = types,
        mod_vars = vars
      }

  findImports :: ModulePath -> [Expr] -> [(ModulePath, Span)]
  findImports mod exprs = foldr (\e acc -> case e of
      Expr {expr = Import mp, pos = p} ->
        -- eliminate self imports (e.g. from prelude)
        if mod == mp
          then acc
          else (mp, p) : acc
      _ -> acc
    ) [] exprs

  findIncludes :: [Expr] -> [(FilePath, Span)]
  findIncludes exprs = foldr (\e acc -> case e of
      Expr {expr = Include ip, pos = p} -> (ip, p) : acc
      _ -> acc
    ) [] exprs
