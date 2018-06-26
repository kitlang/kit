module Kit.Compiler.Context where

  import Kit.Str
  import Kit.Ast.Base

  data CompileContext = CompileContext {
    context_main_module :: ModulePath,
    context_source_paths :: [FilePath],
    context_output_dir :: FilePath,
    context_defines :: [(String, String)]
  } deriving (Eq, Show)

  compile_context = CompileContext {
    context_main_module = ["main"],
    context_source_paths = ["src"],
    context_output_dir = "build",
    context_defines = []
  }
