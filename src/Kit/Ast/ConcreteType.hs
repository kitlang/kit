module Kit.Ast.ConcreteType (
  ConcreteType,
  ConcreteArgs,
  TraitConstraint,
  module Kit.Ast.ConcreteTypeBase
) where

import Kit.Ast.ConcreteTypeBase
import Kit.Ast.TypedExpr

type ConcreteType = ConcreteTypeBase TypedExpr
type TraitConstraint = TraitConstraintBase TypedExpr
type ConcreteArgs = ConcreteArgsBase TypedExpr
