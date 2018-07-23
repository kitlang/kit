module Kit.Compiler.Passes.TypeModuleContent where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import System.FilePath
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Parser
import Kit.Str

{-
  Performs type resolution and type checking; converts Expr into TypedExpr and
  (Maybe TypeSpec) annotations into ConcreteTypes.

  See Kit.Compiler.Typers.* for specific typing implementations.
-}
typeContent :: CompileContext -> IO ()
typeContent ctx = do
  mods <- ctxSourceModules ctx
  forM_ mods (typeModuleContent ctx)

typeModuleContent :: CompileContext -> Module -> IO ()
typeModuleContent ctx mod = do
  defs <- bindingList (modDefinitions mod)
  forM_
    defs
    (\d -> case d of
      DefinitionFunction f -> typeFunction ctx mod f
      DefinitionVar      v -> typeVar ctx mod v
      DefinitionTrait    t -> typeTrait ctx mod t
      DefinitionType     t -> typeTypeDefinition ctx mod t
    )
