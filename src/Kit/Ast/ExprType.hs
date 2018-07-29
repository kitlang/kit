module Kit.Ast.ExprType where

import Kit.Ast.ConcreteType
import Kit.Ast.Identifier
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.Operator
import Kit.Ast.TypeSpec
import Kit.Ast.Value
import Kit.Parser.Span
import Kit.Parser.Token
import Kit.Str

data MatchCase a = MatchCase {match_pattern :: a, match_body :: a} deriving (Eq, Show)

{-
  All AST structures use the convention of two type parameters, a and b.

  a = recursive expression data type (Expr, TypedExpr)
  b = type specifier data type (Maybe TypeSpec, ConcreteType)

  This allows reuse across passes of compilation:

  - Initially, we use `ExprType (Expr) (Maybe TypeSpec)` for expressions with
    only position info and optional annotations
  - Typing converts this to `ExprType (TypedExpr) (ConcreteType)`, where each
    expression has a type and all types are resolved to a single compile-time
    type
  - Finally, we generate an IR of `ExprType (IrExpr) (BasicType)` where the
    allowed AST nodes are much more restricted, compile-time-only features are
    erased, and all types are reduced to their actual storage type.
-}
data ExprType a b
  = Block [a]
  | Meta (Metadata) a
  | Literal ValueLiteral
  | This
  | Self
  -- identifier, namespace
  | Identifier Identifier [Str]
  -- expression, optional type annotation; blank to infer type
  | TypeAnnotation a b
  | PreUnop Operator a
  | PostUnop Operator a
  | Binop Operator a a
  -- for (e1 in e2) e3
  | For a a a
  -- while (e1) e2
  | While a a
  -- if (e1) e2 [else e3]
  | If a a (Maybe a)
  | Continue
  | Break
  | Return (Maybe a)
  | Throw a
  | Match a [MatchCase a] (Maybe a)
  | InlineCall a
  | Field a Identifier
  | StructInit b [(Str, a)]
  | EnumInit b Str [a]
  | ArrayAccess a a
  | Call a [a]
  | Cast a b
  | TokenExpr [TokenClass]
  | Unsafe a
  | BlockComment Str
  | LexMacro Str [TokenClass]
  -- e1 ... e2
  | RangeLiteral a a
  | VectorLiteral [a]
  -- var id[: type] [= default];
  | VarDeclaration Identifier b (Maybe a)
  | Defer a
  deriving (Eq, Show)
