{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Kit.Ast.TypedExpr (
  TypedExpr(..),
  makeExprTyped,
  makeBlock
) where

import           Data.Hashable
import           GHC.Generics
import           Kit.Ast.ConcreteTypeBase
import           Kit.Ast.Definitions
import           Kit.Ast.ExprType
import           Kit.Ast.Span
import           Kit.Ast.Value

type ConcreteType = ConcreteTypeBase TypedExpr

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
  tCompileTimeValue :: Maybe ValueLiteral,
  tIsConst :: Bool,
  tImplicitRules :: [RewriteRule TypedExpr ConcreteType]
} deriving (Generic)

instance Eq TypedExpr where
  (==) a b = (tExpr a == tExpr b)

instance Show TypedExpr where
  show x = "(" ++ (show $ tExpr x) ++ "): " ++ (show $ inferredType x)

instance Positioned TypedExpr where
  position = tPos

instance Hashable TypedExpr where
  hashWithSalt = hashUsing tExpr

makeExprTyped :: TypedExprType -> ConcreteType -> Span -> TypedExpr
makeExprTyped et t pos = TypedExpr
  { tExpr             = et
  , inferredType      = t
  , tImplicits        = []
  , tPos              = pos
  , rewrittenBy       = Nothing
  , tIsLvalue         = False
  , tIsLocal          = False
  , tIsLocalPtr       = False
  , tCompileTimeValue = Nothing
  , tIsConst          = False
  , tImplicitRules    = []
  }

makeBlock :: TypedExpr -> TypedExpr
makeBlock ex@(TypedExpr { tExpr = Block _ }) = ex
makeBlock ex = makeExprTyped (Block [ex]) TypeVoid (tPos ex)
