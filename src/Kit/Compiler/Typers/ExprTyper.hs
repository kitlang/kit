module Kit.Compiler.Typers.ExprTyper where

import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr

data TyperUtils = TyperUtils {
  _r :: TypedExpr -> IO TypedExpr,
  _tryRewrite :: TypedExpr -> IO TypedExpr -> IO TypedExpr,
  _resolve :: TypeConstraint -> IO (),
  _typeExpr :: ExprTyper,
  _maybeR :: Maybe TypedExpr -> IO (Maybe TypedExpr)
}

-- the signature of typeExpr
type ExprTyper = CompileContext -> TypeContext -> Module -> TypedExpr -> IO TypedExpr
-- the signature of a modular subtyper, which also receives utility functions as arguments
type SubTyper =  TyperUtils -> ExprTyper
