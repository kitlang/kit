module Kit.Compiler.TypedExpr where

import Kit.Ast
import Kit.Error
import Kit.Parser.Span

type TypedExprType = ExprType TypedExpr ConcreteType
data TypedExpr = TypedExpr {
  texpr :: TypedExprType,
  tPos :: Span,
  inferredType :: ConcreteType,
  rewrittenBy :: Maybe (RewriteRule (Expr) (Maybe TypeSpec)),
  tErrors :: [KitError]
} deriving (Eq, Show)

makeExprTyped :: TypedExprType -> ConcreteType -> Span -> TypedExpr
makeExprTyped et t pos = TypedExpr
  { texpr        = et
  , inferredType = t
  , tPos         = pos
  , rewrittenBy  = Nothing
  , tErrors      = []
  }
