module Kit.Compiler (
  tryCompile,
  module Kit.Compiler.Context,
  module Kit.Compiler.Module
) where

  import Control.Exception
  import Control.Monad
  import System.Directory
  import System.FilePath
  import Kit.Ast
  import Kit.Compiler.Context
  import Kit.Compiler.Module
  import Kit.Error
  import Kit.Parser
  import Kit.Str

  tryCompile :: CompileContext -> IO (Either Errors ())
  tryCompile context = try $ compile context

  {-
    Run compilation to completion from the given CompileContext. Throws an
    Error on failure.
  -}
  compile :: CompileContext -> IO ()
  compile ctx = do
    parsed <- parseModule ctx (context_main_module ctx)
    return ()

  parseModule :: CompileContext -> ModulePath -> IO Module
  parseModule ctx mod = do
    exprs <- parseModuleExprs ctx mod
    return $ new_mod mod exprs

  parseModuleExprs :: CompileContext -> ModulePath -> IO [Expr]
  parseModuleExprs ctx mod = do
    path <- (findModule ctx mod)
    parsed <- parseFile path
    case parsed of
      ParseResult r -> return r
      Err e -> throw $ Errs [e]

  findModule :: CompileContext -> ModulePath -> IO FilePath
  findModule ctx mod = do
    let modPath = replaceExtension (joinPath (map s_unpack mod)) ".kit"
    matches <- filterM doesFileExist [dir </> modPath | dir <- context_source_paths ctx]
    if matches == []
      then throw $ Errs []
      else return $ matches !! 0
