module Kit.Compiler.TypedExpr where

import Kit.Ast
import Kit.Parser.Span

type TypedExprType = ExprType TypedExpr ConcreteType
data TypedExpr = TypedExpr {texpr :: TypedExprType, tPos :: Span, inferredType :: ConcreteType} deriving (Eq, Show)

makeExprTyped :: TypedExprType -> ConcreteType -> Span -> TypedExpr
makeExprTyped et t pos = TypedExpr { texpr = et, inferredType = t, tPos = pos }
