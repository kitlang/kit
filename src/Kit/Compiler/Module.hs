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
  modInterface :: Scope (),
  modTypes :: Scope TypeBinding,
  modFunctions :: Scope (FunctionDefinition Expr (Maybe TypeSpec)),
  modContents :: IORef [Statement],
  modTypedContents :: Scope TypedDecl,
  modVars :: Scope Binding,
  modIsCModule :: Bool,
  modIr :: IORef [IrDecl],
  modTraits :: Scope (TraitDefinition Expr (Maybe TypeSpec)),
  modImpls :: Scope (HashTable TypePath (TraitImplementation Expr (Maybe TypeSpec)))
}

instance Show Module where
  show m = "<module " ++ (s_unpack $ showModulePath $ modPath m) ++ " (" ++ (modSourcePath m) ++ ")>"

emptyMod :: IO Module
emptyMod = do
  contents      <- newIORef []
  types         <- newScope
  functions     <- newScope
  traits        <- newScope
  vars          <- newScope
  enums         <- newScope
  typedContents <- newScope
  ir            <- newIORef []
  return $ Module
    { modPath          = undefined
    , modSourcePath    = undefined
    , modImports       = []
    , modIncludes      = []
    , modTypes         = types
    , modFunctions     = functions
    , modVars          = vars
    , modContents      = contents
    , modTypedContents = typedContents
    , modIr            = ir
    , modIsCModule     = False
    , modTraits        = traits
    }

newMod :: ModulePath -> [Statement] -> FilePath -> IO Module
newMod path stmts fp = do
  mod <- emptyMod
  writeIORef (modContents mod) (stmts)
  return $ mod { modPath       = path
               , modSourcePath = fp
               , modImports    = _findImports path stmts
               , modIncludes   = _findIncludes stmts
               }

newCMod :: FilePath -> IO Module
newCMod fp = do
  mod <- emptyMod
  return $ mod { modPath       = includeToModulePath fp
               , modSourcePath = fp
               , modIsCModule  = True
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
