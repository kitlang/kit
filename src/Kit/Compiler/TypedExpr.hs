{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Kit.Compiler.TypedExpr where

import Data.Hashable
import GHC.Generics
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
} deriving (Eq, Generic, Show)

instance Hashable TypedExpr
instance Hashable Metadata
instance Hashable MetaArg
instance Hashable Modifier
instance Hashable (Identifier ConcreteType)
instance Hashable (TypeParam ConcreteType)
instance Hashable (ExprType TypedExpr ConcreteType)
instance Hashable (UsingType TypedExpr ConcreteType)
instance Hashable (RewriteRule TypedExpr ConcreteType)
instance Hashable (FunctionDefinition TypedExpr ConcreteType)
instance Hashable (ArgSpec TypedExpr ConcreteType)
instance Hashable (TraitDefinition TypedExpr ConcreteType)
instance Hashable (TraitImplementation TypedExpr ConcreteType)
instance Hashable (MatchCase TypedExpr)

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
