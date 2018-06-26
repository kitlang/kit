module Kit.Compiler.Module where

  import Kit.Ast

  data Module = Module {
    mod_path :: ModulePath,
    mod_contents :: [Expr],
    mod_imports :: [ModulePath],
    mod_types :: [Expr]
  } deriving (Eq, Show)

  new_mod :: ModulePath -> [Expr] -> Module
  new_mod path exprs = Module {
    mod_path = path,
    mod_contents = exprs,
    mod_imports = find_imports exprs,
    mod_types = []
  }

  find_imports :: [Expr] -> [ModulePath]
  find_imports exprs = foldr (\e acc -> case expr e of
      Import mp -> mp : acc
      _ -> acc
    ) [] exprs
