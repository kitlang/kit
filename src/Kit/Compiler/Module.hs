module Kit.Compiler.Module where

  import Kit.Ast
  import Kit.HashTable
  import Kit.Parser.Span
  import Kit.Str

  data Module = Module {
    mod_path :: ModulePath,
    mod_contents :: [Expr],
    mod_imports :: [(ModulePath, Span)],
    mod_types :: HashTable Str Structure,
    mod_vars :: HashTable Str Binding,
    mod_basic_types :: HashTable Str BasicType
  } deriving (Show)

  newMod :: ModulePath -> [Expr] -> IO Module
  newMod path exprs = do
    types <- h_new
    vars <- h_new
    basic_types <- h_new
    return $ Module {
        mod_path = path,
        mod_contents = exprs,
        mod_imports = findImports exprs,
        mod_types = types,
        mod_vars = vars,
        mod_basic_types = basic_types
      }

  findImports :: [Expr] -> [(ModulePath, Span)]
  findImports exprs = foldr (\e acc -> case e of
      Expr {expr = Import mp, pos = p} -> (mp, p) : acc
      _ -> acc
    ) [] exprs
