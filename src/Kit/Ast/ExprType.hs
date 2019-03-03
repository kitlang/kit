{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Kit.Ast.ExprType where

import Data.Hashable
import GHC.Generics
import Kit.Ast.Definitions
import Kit.Ast.Identifier
import Kit.Ast.Metadata
import Kit.Ast.Operator
import Kit.Ast.TypePath
import Kit.Ast.UsingType
import Kit.Ast.Value
import Kit.Error
import Kit.Parser.Token
import Kit.Str

data MatchCase a = MatchCase {matchPattern :: a, matchBody :: a} deriving (Eq, Generic, Show)

instance (Hashable a) => Hashable (MatchCase a)

newMatchCase = MatchCase {matchPattern = undefined, matchBody = undefined}
matchCase x y = MatchCase {matchPattern = x, matchBody = y}

{-
  All AST structures use the convention of two type parameters, a and b.

  a = recursive expression data type (Expr, TypedExpr)
  b = type specifier data type (TypeSpec, ConcreteType)

  This allows reuse across passes of compilation:

  - Initially, we use `ExprType (Expr) (TypeSpec)` for expressions with only
    position info and optional annotations
  - Typing converts this to `ExprType (TypedExpr) (ConcreteType)`, where each
    expression has a type and all types are resolved to a single compile-time
    type
-}
data ExprType a b
  = Block [a]
  | Using [UsingType a b] a
  | Meta Metadata a
  | Literal ValueLiteral b
  | This
  | Self
  -- identifier, namespace
  | Identifier (Identifier b)
  -- expression, optional type annotation; blank to infer type
  | TypeAnnotation a b
  | PreUnop Operator a
  | PostUnop Operator a
  | Binop Operator a a
  -- for (e1 in e2) e3
  | For a a a
  -- while (e1) e2 (do?)
  | While a a Bool
  -- if (e1) e2 [else e3]
  | If a a (Maybe a)
  | Continue
  | Break
  | Return (Maybe a)
  | Match a [MatchCase a] (Maybe a)
  | InlineCall a
  | Field a (Identifier b)
  | StructInit b [(Str, a)]
  | UnionInit b (Str, a)
  | EnumInit b TypePath [(Str, a)]
  | EnumField a Str Str
  | TupleInit [a]
  | TupleSlot a Int
  | ArrayAccess a a
  | Call a [a] [a]
  | Cast a b
  | Unsafe a
  | BlockComment Str
  -- e1 ... e2
  | RangeLiteral a a
  | ArrayLiteral [a]
  -- var id[: type] [= default];
  | LocalVarDeclaration (Identifier b) b Bool (Maybe a)
  -- | Defer a
  | Box (TraitImplementation a b) a
  | BoxedValue a
  | BoxedVtable (TraitDefinition a b) a
  | StaticVtable (TraitImplementation a b)
  | SizeOf b
  | StaticMember TypePath [b] Str
  | Implicit b
  | Temp a
  | Null
  | Empty
  | Undefined
  | InlineCExpr Str b
  | VarArg Str
  | VarArgListCopy Str
  | StaticExpr a
  | Yield a
  | Tokens Str
  | Defined (Identifier b)
  deriving (Eq, Generic, Show)

instance (Hashable a, Hashable b) => Hashable (ExprType a b)

exprDiscriminant :: (Show a, Show b) => ExprType a b -> Int
exprDiscriminant et = case et of
  Block _                     -> 1
  Using   _ _                 -> 1
  Meta    _ _                 -> 3
  Literal _ _                 -> 4
  This                        -> 5
  Self                        -> 6
  Identifier _                -> 7
  TypeAnnotation _ _          -> 8
  PreUnop        _ _          -> 9
  PostUnop       _ _          -> 10
  Binop _ _ _                 -> 11
  For   _ _ _                 -> 12
  While _ _ _                 -> 13
  If    _ _ _                 -> 14
  Continue                    -> 15
  Break                       -> 16
  Return     _                -> 17
  StaticExpr _                -> 18
  Match _ _ _                 -> 19
  InlineCall _                -> 20
  Field      _ _              -> 21
  StructInit _ _              -> 22
  EnumInit  _ _ _             -> 23
  EnumField _ _ _             -> 40
  ArrayAccess _ _             -> 24
  Call _ _ _                  -> 25
  Cast _ _                    -> 26
  Unsafe       _              -> 27
  BlockComment _              -> 28
  RangeLiteral _ _            -> 29
  ArrayLiteral _              -> 30
  LocalVarDeclaration _ _ _ _ -> 31
  -- Defer _                -> 32
  Box _ _                     -> 33
  BoxedValue _                -> 34
  BoxedVtable _ _             -> 35
  SizeOf    _                 -> 36
  TupleInit _                 -> 37
  TupleSlot _ _               -> 38
  StaticMember _ _ _          -> 39
  Implicit _                  -> 40
  Temp     _                  -> 41
  Null                        -> 42
  Empty                       -> 43
  InlineCExpr _ _             -> 44
  VarArg     _                -> 45
  StaticExpr _                -> 46
  Yield      _                -> 47
  Tokens     _                -> 48
  Undefined                   -> 49
  UnionInit _ _               -> 50
  VarArgListCopy _            -> 51
  StaticVtable   _            -> 52
  Defined _                   -> 53
  x                           -> throwk
    $ InternalError ("Expression has no discriminant: " ++ show x) Nothing

exprChildren :: ExprType a b -> [a]
exprChildren et = case et of
  Block x               -> x
  Using          _ x    -> [x]
  Meta           _ x    -> [x]
  TypeAnnotation x _    -> [x]
  PreUnop        _ x    -> [x]
  PostUnop       _ x    -> [x]
  Binop _ x y           -> [x, y]
  For   x y z           -> [x, y, z]
  While x y _           -> [x, y]
  If    x y (Just z)    -> [x, y, z]
  If    x y Nothing     -> [x, y]
  Match x _ _           -> [x]
  InlineCall x          -> [x]
  Field      x _        -> [x]
  StructInit _ fields   -> map snd fields
  UnionInit  _ field    -> [snd field]
  EnumInit  _ _ x       -> map snd x
  EnumField x _ _       -> [x]
  ArrayAccess x y       -> [x, y]
  Call x implicits args -> x : implicits ++ args
  Cast x _              -> [x]
  Unsafe x              -> [x]
  RangeLiteral x y      -> [x, y]
  ArrayLiteral x        -> x
  LocalVarDeclaration _ _ _ (Just x) -> [x]
  -- Defer x                       -> [x]
  Box _ x               -> [x]
  BoxedValue x          -> [x]
  BoxedVtable _ x       -> [x]
  TupleInit x           -> x
  TupleSlot x _         -> [x]
  Temp       x          -> [x]
  StaticExpr x          -> [x]
  Yield      x          -> [x]
  _                     -> []

exprMapReduce :: (a -> c) -> (c -> d -> d) -> (a -> ExprType a b) -> d -> a -> d
exprMapReduce mapper reducer getter initialValue ex = foldr
  (\a d -> exprMapReduce mapper reducer getter d a)
  (reducer (mapper ex) (initialValue))
  (exprChildren $ getter ex)

exprFilterMapReduce
  :: (a -> Bool)
  -> (a -> c)
  -> (c -> d -> d)
  -> (a -> ExprType a b)
  -> d
  -> a
  -> d
exprFilterMapReduce f mapper reducer getter initialValue ex = foldr
  (\a d -> exprFilterMapReduce f mapper reducer getter d a)
  (reducer (mapper ex) (initialValue))
  (if f ex then exprChildren $ getter ex else [])
