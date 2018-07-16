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

data Module = Module {
  modPath :: ModulePath,
  modSourcePath :: FilePath,
  modImports :: [(ModulePath, Span)],
  modIncludes :: [(FilePath, Span)],
  modTypes :: Scope ConcreteType,
  modTypeDefinitions :: Scope (TypeDefinition Expr (Maybe TypeSpec)),
  modFunctions :: Scope (FunctionDefinition Expr (Maybe TypeSpec)),
  modContents :: IORef [Statement],
  modTypedContents :: Scope TypedDecl,
  modVars :: Scope Binding,
  modIsCModule :: Bool,
  modIr :: IORef [IrDecl]
}

instance Show Module where
  show m = "<module " ++ (s_unpack $ showModulePath $ modPath m) ++ " (" ++ (modSourcePath m) ++ ")>"

newMod :: ModulePath -> [Statement] -> FilePath -> IO Module
newMod path stmts fp = do
  contents        <- newIORef stmts
  types           <- newScope
  typeDefinitions <- newScope
  functions       <- newScope
  vars            <- newScope
  enums           <- newScope
  typedContents   <- newScope
  ir              <- newIORef []
  return $ Module
    { modPath            = path
    , modSourcePath      = fp
    , modImports         = _findImports path stmts
    , modIncludes        = _findIncludes stmts
    , modTypes           = types
    , modTypeDefinitions = typeDefinitions
    , modFunctions       = functions
    , modVars            = vars
    , modContents        = contents
    , modTypedContents   = typedContents
    , modIr              = ir
    , modIsCModule       = False
    }

newCMod :: FilePath -> IO Module
newCMod fp = do
  contents        <- newIORef []
  types           <- newScope
  typeDefinitions <- newScope
  functions       <- newScope
  vars            <- newScope
  enums           <- newScope
  typedContents   <- newScope
  ir              <- newIORef []
  return $ Module
    { modPath            = includeToModulePath fp
    , modSourcePath      = fp
    , modImports         = []
    , modIncludes        = []
    , modTypes           = types
    , modTypeDefinitions = typeDefinitions
    , modFunctions       = functions
    , modVars            = vars
    , modContents        = contents
    , modTypedContents   = typedContents
    , modIr              = ir
    , modIsCModule       = True
    }

_findImports :: ModulePath -> [Statement] -> [(ModulePath, Span)]
_findImports mod stmts = foldr
  (\e acc -> case e of
    Statement { stmt = Import mp, stmtPos = p } ->
      -- eliminate self imports (e.g. from prelude)
      if mod == mp then acc else (mp, p) : acc
    _ -> acc
  )
  []
  stmts

_findIncludes :: [Statement] -> [(FilePath, Span)]
_findIncludes stmts = foldr
  (\e acc -> case e of
    Statement { stmt = Include ip, stmtPos = p } -> (ip, p) : acc
    _ -> acc
  )
  []
  stmts

includeToModulePath :: FilePath -> ModulePath
includeToModulePath fp = "c" : (map s_pack $ splitDirectories (fp -<.> ""))
