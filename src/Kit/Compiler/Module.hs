module Kit.Compiler.Module where

  import Kit.Ast
  import Kit.Parser.Span

  data Module = Module {
    mod_path :: ModulePath,
    mod_contents :: [Expr],
    mod_imports :: [(ModulePath, Span)],
    mod_types :: [Expr]
  } deriving (Eq, Show)

  new_mod :: ModulePath -> [Expr] -> Module
  new_mod path exprs = Module {
    mod_path = path,
    mod_contents = exprs,
    mod_imports = find_imports exprs,
    mod_types = []
  }

  find_imports :: [Expr] -> [(ModulePath, Span)]
  find_imports exprs = foldr (\e acc -> case e of
      Expr {expr = Import mp, pos = p} -> (mp, p) : acc
      _ -> acc
    ) [] exprs
