module Kit.Compiler.Passes.GenerateTypes where

  import Control.Exception
  import Control.Monad
  import Data.IORef
  import Data.List
  import System.Directory
  import System.FilePath
  import Kit.Ast
  import Kit.Compiler.Context
  import Kit.Compiler.Module
  import Kit.Compiler.Scope
  import Kit.Compiler.Typer
  import Kit.Compiler.TypeUsage
  import Kit.Compiler.Utils
  import Kit.Error
  import Kit.HashTable
  import Kit.Parser
  import Kit.Str

  generateTypes :: CompileContext -> IO ()
  generateTypes ctx = do
    mods <- h_toList $ context_modules ctx
    forM (map snd mods) (generateModuleTypes ctx)
    return ()

  generateModuleTypes :: CompileContext -> Module -> IO ()
  generateModuleTypes ctx mod = do
    types <- bindingList $ mod_types mod
    forM types (generateType ctx mod)
    return ()

  generateType :: CompileContext -> Module -> TypeUsage -> IO ()
  generateType ctx mod usage = do
    typeMonomorphs <- h_toList $ monomorphs usage
    forM typeMonomorphs (monomorphize ctx mod (type_definition usage))
    return ()

  monomorphize :: CompileContext -> Module -> TypeDefinition -> (Str, [TypeSpec]) -> IO ()
  monomorphize ctx mod t@(TypeDefinition {type_type = Struct {}}) params = do
    -- TODO
    return ()
  monomorphize ctx mod t@(TypeDefinition {type_type = Enum {}}) params = do
    -- TODO
    return ()
  monomorphize ctx mod _ params = do
    return ()
