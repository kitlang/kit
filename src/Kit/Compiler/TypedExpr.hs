module Kit.Compiler.TypedExpr where

import Kit.Ast
import Kit.Ast.Span

type TypedExprType = ExprType TypedExpr ConcreteType
data TypedExpr = TypedExpr {
  tExpr :: TypedExprType,
  inferredType :: ConcreteType,
  tImplicits :: [TypedExpr],
  tPos :: Span,
  rewrittenBy :: Maybe (RewriteRule TypedExpr ConcreteType),
  tIsLvalue :: Bool,
  tIsLocal :: Bool,
  tIsLocalPtr :: Bool,
  tIsCompileTime :: Bool,
  tIsConst :: Bool,
  tImplicitRules :: [RewriteRule TypedExpr ConcreteType]
  -- tSubTypes :: [ConcreteType]
} deriving (Eq, Show)

makeExprTyped :: TypedExprType -> ConcreteType -> Span -> TypedExpr
makeExprTyped et t pos = TypedExpr
  { tExpr          = et
  , inferredType   = t
  , tImplicits     = []
  , tPos           = pos
  , rewrittenBy    = Nothing
  , tIsLvalue      = False
  , tIsLocal       = False
  , tIsLocalPtr    = False
  , tIsCompileTime = False
  , tIsConst       = False
  , tImplicitRules = []
  }
