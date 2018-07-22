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
-}
data ExprType a b
  = Block [a]
  | Meta (Metadata) a
  | Literal ValueLiteral
  | This
  | Self
  -- identifier, nameMangling
  | Identifier Identifier (Maybe ModulePath)
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
  | ArrayAccess a a
  | Call a [a]
  | Cast a b
  | TokenExpr [TokenClass]
  | Unsafe a
  | BlockComment Str
  | New b [a]
  | Copy a
  | Delete a
  | Move a
  | LexMacro Str [TokenClass]
  -- e1 ... e2
  | RangeLiteral a a
  | VectorLiteral [a]
  -- var id[: type] [= default];
  | VarDeclaration Identifier b (Maybe a)
  deriving (Eq, Show)
