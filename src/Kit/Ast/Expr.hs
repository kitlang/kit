{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module Kit.Ast.Expr where

import Data.Hashable
import GHC.Generics
import Kit.Ast.ExprType
import Kit.Ast.Span
import Kit.Ast.Statement
import Kit.Ast.Types

type SyntacticStatement = Statement Expr (Maybe TypeSpec)
type SyntacticExprType = ExprType Expr (Maybe TypeSpec)

data Expr = Expr {expr :: SyntacticExprType, pos :: Span} deriving (Show, Generic)
instance Eq Expr where
  (==) a b = (expr a) == (expr b)
instance Hashable Expr where
  hashWithSalt = hashUsing expr

ePos = pos

e :: SyntacticExprType -> Expr
e et = ep et NoPos

ep :: SyntacticExprType -> Span -> Expr
ep et p = Expr {expr = et, pos = p}

pe :: Span -> SyntacticExprType -> Expr
pe p et = ep et p

me :: Span -> Expr -> Expr
me p ex = Expr {expr = expr ex, pos = p}

instance Positioned Expr where
  position = pos
