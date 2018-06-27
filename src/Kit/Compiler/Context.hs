module Kit.Compiler.Context where

  import Kit.Ast.Base
  import Kit.Compiler.Module
  import Kit.Hash
  import Kit.Str

  data CompileContext = CompileContext {
    context_main_module :: ModulePath,
    context_source_paths :: [FilePath],
    context_output_dir :: FilePath,
    context_defines :: [(String, String)],
    context_modules :: HashTable ModulePath Module
  } deriving (Show)

  compile_context = CompileContext {
    context_main_module = ["main"],
    context_source_paths = ["src"],
    context_output_dir = "build",
    context_defines = [],
    context_modules = undefined
  }
