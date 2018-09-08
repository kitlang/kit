module Kit.Compiler.Module where

import Control.Monad
import Data.IORef
import System.FilePath
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.TypedExpr
import Kit.Error
import Kit.HashTable
import Kit.Ir
import Kit.Log
import Kit.Parser.Span
import Kit.Str

data DuplicateDeclarationError = DuplicateDeclarationError ModulePath Str Span Span deriving (Eq, Show)
instance Errable DuplicateDeclarationError where
  logError e@(DuplicateDeclarationError mod name pos1 pos2) = do
    logErrorBasic e $ "Duplicate declaration for `" ++ s_unpack name ++ "` in " ++ s_unpack (showModulePath mod) ++ "; \n\nFirst declaration:"
    ePutStrLn "\nSecond declaration:"
    displayFileSnippet pos2
    ePutStrLn "\nFunction, variable, type and trait names must be unique within the same namespace."
  errPos (DuplicateDeclarationError _ _ pos _) = Just pos

data Module = Module {
  modPath :: ModulePath,
  modSourcePath :: FilePath,
  modImports :: [(ModulePath, Span)],
  modIncludes :: IORef [(FilePath, Span)],
  modImpls :: IORef [TraitImplementation Expr (Maybe TypeSpec)],
  modSpecializations :: IORef [((TypeSpec, TypeSpec), Span)],
  modUsing :: IORef [UsingType TypedExpr ConcreteType],
  modTuples :: HashTable String BasicType,
  modIsCModule :: Bool
}

instance Show Module where
  show m = "<module " ++ (s_unpack $ showModulePath $ modPath m) ++ " (" ++ (modSourcePath m) ++ ")>"

newMod :: ModulePath -> FilePath -> IO Module
newMod path fp = do
  impls    <- newIORef []
  specs    <- newIORef []
  includes <- newIORef []
  using    <- newIORef []
  tuples   <- h_new
  return $ Module
    { modPath            = path
    , modSourcePath      = fp
    , modImports         = []
    , modIncludes        = includes
    , modImpls           = impls
    , modSpecializations = specs
    , modUsing           = using
    , modTuples          = tuples
    , modIsCModule       = False
    }

emptyMod = newMod [] undefined

externModPath :: ModulePath
externModPath = []

newCMod :: IO Module
newCMod = do
  mod <- newMod externModPath "(extern)"
  return $ mod { modIsCModule = True }

-- includeToModulePath :: FilePath -> ModulePath
-- includeToModulePath fp = "extern" : (map s_pack $ splitDirectories (fp -<.> ""))
