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
  modImpls :: IORef [TraitImplementation Expr (Maybe TypeSpec)],
  modSpecializations :: IORef [((TypeSpec, TypeSpec), Span)],
  modContents :: Scope (Declaration Expr (Maybe TypeSpec)),
  modTypedContents :: Scope TypedDecl,
  modIr :: IORef [IrDecl],
  modIsCModule :: Bool
}

instance Show Module where
  show m = "<module " ++ (s_unpack $ showModulePath $ modPath m) ++ " (" ++ (modSourcePath m) ++ ")>"

emptyMod :: IO Module
emptyMod = do
  scope         <- newScope
  defs          <- newScope
  traits        <- newScope
  impls         <- newIORef []
  specs         <- newIORef []
  typedContents <- newScope
  ir            <- newIORef []
  includes      <- newIORef []
  return $ Module
    { modPath            = undefined
    , modSourcePath      = undefined
    , modImports         = []
    , modIncludes        = includes
    , modScope           = scope
    , modContents     = defs
    , modImpls           = impls
    , modSpecializations = specs
    , modTypedContents   = typedContents
    , modIr              = ir
    , modIsCModule       = False
    }

newMod :: ModulePath -> FilePath -> IO Module
newMod path fp = do
  mod <- emptyMod
  return $ mod { modPath = path, modSourcePath = fp }

newCMod :: FilePath -> IO Module
newCMod fp = do
  mod <- emptyMod
  return $ mod { modPath       = includeToModulePath fp
               , modSourcePath = fp
               , modIsCModule  = True
               }

includeToModulePath :: FilePath -> ModulePath
includeToModulePath fp = "c" : (map s_pack $ splitDirectories (fp -<.> ""))
