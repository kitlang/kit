module Kit.Compiler.TypedExpr where

import Kit.Ast
import Kit.Parser.Span

data TypedExpr = TypedExpr {texpr :: ExprType TypedExpr, tPos :: Span, inferredType :: ConcreteType} deriving (Eq, Show)

instance ExprWrapper TypedExpr where
  getExpr = texpr
  makeExpr et = TypedExpr {texpr = et, tPos = null_span, inferredType = undefined}
  setExpr ex et = ex {texpr = et}

makeExprTyped :: ExprType TypedExpr -> ConcreteType -> Span -> TypedExpr
makeExprTyped et t pos = (makeExpr et) { inferredType = t, tPos = pos }
