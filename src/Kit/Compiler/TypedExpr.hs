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
  tIsLvalue :: Bool,
  tTemps :: [TypedExpr]
  -- tSubTypes :: [ConcreteType]
} deriving (Eq, Show)

makeExprTyped :: TypedExprType -> ConcreteType -> Span -> TypedExpr
makeExprTyped et t pos = TypedExpr
  { tExpr        = et
  , inferredType = t
  , tImplicits   = []
  , tPos         = pos
  , rewrittenBy  = Nothing
  , tIsLvalue    = False
  , tTemps       = []
  }
