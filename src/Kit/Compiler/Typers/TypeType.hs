module Kit.Compiler.Typers.TypeType where

import Control.Monad
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.Base
import Kit.Compiler.Unify
import Kit.Error
import Kit.Parser
import Kit.Str

typeTypeDefinition
  :: CompileContext
  -> Module
  -> TypeDefinition Expr (Maybe TypeSpec)
  -> IO ()
typeTypeDefinition ctx mod def = do
  -- TODO
  return ()
