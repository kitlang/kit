module Kit.Compiler.Module where

import Control.Monad
import Data.Mutable
import Data.Maybe
import Kit.Ast
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Ast.Span
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
  modPath :: !ModulePath,
  modSourcePath :: FilePath,
  modImports :: ![(ModulePath, Span)],
  modIncludes :: IORef [(FilePath, Span)],
  modDefaults :: IORef [((TypeSpec, TypeSpec), Span)],
  modUsing :: IORef [UsingType TypedExpr ConcreteType],
  modIsCModule :: !Bool
}

instance Show Module where
  show m = "<module " ++ (s_unpack $ showModulePath $ modPath m) ++ " (" ++ (modSourcePath m) ++ ")>"

newMod :: ModulePath -> FilePath -> IO Module
newMod path fp = do
  defaults <- newRef []
  includes <- newRef []
  using    <- newRef []
  return $ Module
    { modPath       = path
    , modSourcePath = fp
    , modImports    = []
    , modIncludes   = includes
    , modDefaults   = defaults
    , modUsing      = using
    , modIsCModule  = False
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

modImplicits :: Module -> IO [TypedExpr]
modImplicits mod = do
  usings <- readRef $ modUsing mod
  return $ catMaybes
    [ case u of
        UsingImplicit i -> Just i
        _               -> Nothing
    | u <- usings
    ]

modImportPaths :: Module -> [ModulePath]
modImportPaths mod = (modPath mod) : ((map fst $ modImports mod) ++ [[]])
