module Kit.Compiler.Module where

import Data.IORef
import System.FilePath
import Kit.Ast
import Kit.Compiler.Binding
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
  modImpls :: IORef [TraitImplementation Expr (Maybe TypeSpec)],
  modSpecializations :: IORef [((TypeSpec, TypeSpec), Span)],
  modUsing :: IORef [UsingType Expr (Maybe TypeSpec)],
  modIsCModule :: Bool
}

instance Show Module where
  show m = "<module " ++ (s_unpack $ showModulePath $ modPath m) ++ " (" ++ (modSourcePath m) ++ ")>"

newMod :: ModulePath -> FilePath -> IO Module
newMod path fp = do
  scope         <- newScope path
  impls         <- newIORef []
  specs         <- newIORef []
  includes      <- newIORef []
  using         <- newIORef []
  return $ Module
    { modPath            = path
    , modSourcePath      = fp
    , modImports         = []
    , modIncludes        = includes
    , modScope           = scope
    , modImpls           = impls
    , modSpecializations = specs
    , modUsing           = using
    , modIsCModule       = False
    }

emptyMod = newMod [] undefined

newCMod :: FilePath -> IO Module
newCMod fp = do
  mod <- newMod (includeToModulePath fp) fp
  return $ mod { modIsCModule = True }

includeToModulePath :: FilePath -> ModulePath
includeToModulePath fp = "c" : (map s_pack $ splitDirectories (fp -<.> ""))
