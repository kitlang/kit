module Kit.Ast.ExprType where

import Kit.Ast.ConcreteType
import Kit.Ast.Lvalue
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.Operator
import Kit.Ast.TypeSpec
import Kit.Ast.Value
import Kit.Parser.Span
import Kit.Parser.Token
import Kit.Str

data MatchCase a = MatchCase {match_pattern :: a, match_body :: a} deriving (Eq, Show)

data ExprType a b
  = Block [a]
  | Meta (Metadata) a
  | Literal ValueLiteral
  | This
  | Self
  | Lvalue Lvalue
  | TypeAnnotation a b
  | PreUnop Operator a
  | PostUnop Operator a
  | Binop Operator a a
  | For a a a
  | While a a
  | If a a (Maybe a)
  | Continue
  | Break
  | Return (Maybe a)
  | Throw a
  | Match a [MatchCase a] (Maybe a)
  | InlineCall a
  | Field a Lvalue
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
  | RangeLiteral a a
  | VectorLiteral [a]
  | VarDeclaration Lvalue b (Maybe a)
  deriving (Eq, Show)
