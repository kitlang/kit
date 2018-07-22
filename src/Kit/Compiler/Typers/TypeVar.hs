module Kit.Compiler.Typers.TypeVar where

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

typeVar
  :: CompileContext
  -> Module
  -> VarDefinition Expr (Maybe TypeSpec)
  -> IO ()
typeVar ctx mod def = do
  -- TODO
  return ()
