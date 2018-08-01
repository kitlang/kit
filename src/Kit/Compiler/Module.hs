module Kit.Compiler.Module where

import Data.IORef
import System.FilePath
import Kit.Ast
import Kit.Compiler.Scope
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.HashTable
import Kit.Ir
import Kit.Parser.Span
import Kit.Str

data ModuleInterfaceType
  = ModuleType
  | ModuleVar
  | ModuleFunction
  | ModuleTrait

data Module = Module {
  modPath :: ModulePath,
  modSourcePath :: FilePath,
  modImports :: [(ModulePath, Span)],
  modIncludes :: IORef [(FilePath, Span)],
  modScope :: Scope Binding,
  modContents :: HashTable Str (Declaration Expr (Maybe TypeSpec)),
  modImpls :: IORef [TraitImplementation Expr (Maybe TypeSpec)],
  modSpecializations :: IORef [((TypeSpec, TypeSpec), Span)],
  modTypedContents :: IORef [TypedDecl],
  modIr :: IORef [IrDecl],
  modIsCModule :: Bool
}

instance Show Module where
  show m = "<module " ++ (s_unpack $ showModulePath $ modPath m) ++ " (" ++ (modSourcePath m) ++ ")>"

newMod :: ModulePath -> FilePath -> IO Module
newMod path fp = do
  scope         <- newScope path
  defs          <- h_new
  impls         <- newIORef []
  specs         <- newIORef []
  typedContents <- newIORef []
  ir            <- newIORef []
  includes      <- newIORef []
  return $ Module
    { modPath            = path
    , modSourcePath      = fp
    , modImports         = []
    , modIncludes        = includes
    , modScope           = scope
    , modContents        = defs
    , modImpls           = impls
    , modSpecializations = specs
    , modTypedContents   = typedContents
    , modIr              = ir
    , modIsCModule       = False
    }

emptyMod = newMod [] undefined

newCMod :: FilePath -> IO Module
newCMod fp = do
  mod <- newMod (includeToModulePath fp) fp
  return $ mod { modIsCModule = True }

includeToModulePath :: FilePath -> ModulePath
includeToModulePath fp = "c" : (map s_pack $ splitDirectories (fp -<.> ""))
