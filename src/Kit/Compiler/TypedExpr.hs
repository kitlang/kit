module Kit.Compiler.TypedExpr where

import Kit.Ast
import Kit.Error
import Kit.Parser.Span

type TypedExprType = ExprType TypedExpr ConcreteType
data TypedExpr = TypedExpr {
  tExpr :: TypedExprType,
  inferredType :: ConcreteType,
  tImplicits :: [TypedExpr],
  tPos :: Span,
  rewrittenBy :: Maybe (RewriteRule (Expr) (Maybe TypeSpec)),
  tError :: Maybe KitError
} deriving (Eq, Show)

makeExprTyped :: TypedExprType -> ConcreteType -> Span -> TypedExpr
makeExprTyped et t pos = TypedExpr
  { tExpr        = et
  , inferredType = t
  , tImplicits   = []
  , tPos         = pos
  , rewrittenBy  = Nothing
  , tError       = Nothing
  }

addRef :: TypedExpr -> TypedExpr
addRef ex =
  makeExprTyped (PreUnop Ref ex) (TypePtr $ inferredType ex) (tPos ex)
addDeref :: TypedExpr -> TypedExpr
addDeref ex = case inferredType ex of
  TypePtr x -> makeExprTyped (PreUnop Deref ex) x (tPos ex)
