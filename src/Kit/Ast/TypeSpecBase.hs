{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.TypeSpecBase where

import Control.Monad
import Data.Hashable
import Data.List
import GHC.Generics
import Kit.Ast.BasicType
import Kit.Ast.ModulePath
import Kit.Ast.Span
import Kit.Ast.TypePath
import Kit.Ast.Value
import Kit.Str

{-
  A TypeSpecBase is a syntactic type as specified by a program. TypeSpecBases will be
  resolved to a specific ConcreteType when expressions are typed.
-}
data TypeSpecBase b
  = TypeSpec TypePath [TypeSpecBase b] Span
  | ConstantTypeSpec ValueLiteral Span
  | TupleTypeSpec [TypeSpecBase b] Span
  | PointerTypeSpec (TypeSpecBase b) Span
  | FunctionTypeSpec (TypeSpecBase b) [TypeSpecBase b] (Maybe Str) Span
  {-
    This constructor can be used to force the TypeSpecBase to resolve to a specific
    ConcreteType without going through normal namespace resolution. This is
    used when we already know the underlying type when generating the AST,
    e.g. for C externs.
  -}
  | ConcreteType b
  | InferredType Span
  deriving (Generic)

makeTypeSpec s = TypeSpec ([], s) [] NoPos

instance Positioned (TypeSpecBase b) where
  position (TypeSpec _ _ pos          ) = pos
  position (PointerTypeSpec t pos     ) = pos
  position (FunctionTypeSpec t _ _ pos) = pos
  position (TupleTypeSpec _ pos       ) = pos
  position (ConcreteType _            ) = NoPos
  position (ConstantTypeSpec _ pos    ) = pos
  position (InferredType pos          ) = pos

instance (Show b) => Show (TypeSpecBase b) where
  show (TypeSpec (tp) params _) = (s_unpack $ showTypePath tp) ++ (if null params then "" else "[" ++ (intercalate "," [show param | param <- params]) ++ "]")
  show (ConstantTypeSpec v _) = show v
  show (PointerTypeSpec t _) = "&" ++ show t
  show (TupleTypeSpec t _) = "(" ++ intercalate ", " (map show t) ++ ")"
  show (FunctionTypeSpec t args var _) = "(" ++ intercalate ", " (map show args) ++ (case var of {Just x -> ", " ++ s_unpack x ++ "..."; Nothing -> ""}) ++ ") -> " ++ show t
  show (ConcreteType ct) = show ct
  show (InferredType pos) = "???"

instance (Eq b) => Eq (TypeSpecBase b) where
  (==) (TypeSpec tp1 params1 _) (TypeSpec tp2 params2 _) = (tp1 == tp2) && (params1 == params2)
  (==) (TupleTypeSpec t1 _) (TupleTypeSpec t2 _) = (t1 == t2)
  (==) (ConstantTypeSpec v1 _) (ConstantTypeSpec v2 _) = (v1 == v2)
  (==) (PointerTypeSpec t1 _) (PointerTypeSpec t2 _) = (t1 == t2)
  (==) (FunctionTypeSpec tp1 params1 args1 v1) (FunctionTypeSpec tp2 params2 args2 v2) = (tp1 == tp2) && (params1 == params2) && (args1 == args2) && (v1 == v2)
  (==) (ConcreteType ct1) (ConcreteType ct2) = ct1 == ct2
  (==) (InferredType a) (InferredType b) = a == b
  (==) a b = False

instance (Hashable b) => Hashable (TypeSpecBase b)
