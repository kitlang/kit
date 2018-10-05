{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Kit.Ast.Expr where

import Kit.Ast.ExprType
import Kit.Ast.Types
import Kit.Ast.Span

type SyntacticExprType = ExprType Expr (Maybe TypeSpec)

data Expr = Expr {expr :: SyntacticExprType, pos :: Span} deriving (Eq, Show)
ePos = pos

e :: SyntacticExprType -> Expr
e et = ep et NoPos

ep :: SyntacticExprType -> Span -> Expr
ep et p = Expr {expr = et, pos = p}

pe :: Span -> SyntacticExprType -> Expr
pe p et = ep et p

me :: Span -> Expr -> Expr
me p ex = Expr {expr = expr ex, pos = p}
