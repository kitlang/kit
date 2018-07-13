{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Kit.Ast.Expr where

import Data.Traversable
import Kit.Ast.ConcreteType
import Kit.Ast.Lvalue
import Kit.Ast.Modifier
import Kit.Ast.Operator
import Kit.Ast.TypeSpec
import Kit.Ast.Value
import Kit.Parser.Span
import Kit.Parser.Token
import Kit.Str

class ExprWrapper a where
  getExpr :: a -> ExprType a
  setExpr :: a -> ExprType a -> a
  makeExpr :: ExprType a -> a

data Expr = Expr {expr :: ExprType Expr, pos :: Span} deriving (Show)
instance Eq Expr where
  (==) a b = (expr a) == (expr b) && (pos a == pos b || pos a == null_span || pos b == null_span)
instance ExprWrapper Expr where
  getExpr = expr
  setExpr ex et = ex {expr = et}
  makeExpr = e

e :: ExprType Expr -> Expr
e et = ep et null_span

ep :: ExprType Expr -> Span -> Expr
ep et p = Expr {expr = et, pos = p}

pe :: Span -> ExprType Expr -> Expr
pe p et = ep et p

me :: Span -> Expr -> Expr
me p ex = Expr {expr = expr ex, pos = p}

data MatchCase a = MatchCase {match_pattern :: a, match_body :: a} deriving (Eq, Show)
data Metadata = Metadata {meta_name :: Str, meta_args :: [Expr]} deriving (Eq, Show)

meta s = (Metadata {meta_name = s, meta_args = []} :: Metadata)
metaExtern = meta "extern"

data ExprType a
  = Block [a]
  | Meta (Metadata) a
  | Literal ValueLiteral
  | This
  | Self
  | Lvalue Lvalue
  | TypeAnnotation a TypeSpec
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
  | Cast a TypeSpec
  | TokenExpr [TokenClass]
  | Unsafe a
  | BlockComment Str
  | New TypeSpec [a]
  | Copy a
  | Delete a
  | Move a
  | LexMacro Str [TokenClass]
  | RangeLiteral a a
  | VectorLiteral [a]
  | VarDeclaration Lvalue (Maybe TypeSpec) (Maybe a)
  deriving (Eq, Show)

exprMap :: (ExprWrapper a) => (a -> a) -> a -> a
exprMap f ex = f $ setExpr ex $ case getExpr ex of
  (Block children      ) -> (Block (map (exprMap f) children))
  (Meta m e1           ) -> (Meta m ((exprMap f) e1))
  (Literal l           ) -> (Literal l)
  (This                ) -> (This)
  (Self                ) -> (Self)
  (Lvalue v            ) -> (Lvalue v)
  (TypeAnnotation e1 t ) -> (TypeAnnotation ((exprMap f) e1) t)
  (PreUnop        op e1) -> (PreUnop op ((exprMap f) e1))
  (PostUnop       op e1) -> (PostUnop op ((exprMap f) e1))
  (Binop op e1 e2      ) -> (Binop op ((exprMap f) e1) ((exprMap f) e2))
  (For e1 e2 e3) -> (For ((exprMap f) e1) ((exprMap f) e2) ((exprMap f) e3))
  (While e1 e2         ) -> (While ((exprMap f) e1) ((exprMap f) e2))
  (If e1 e2 e3) ->
    (If ((exprMap f) e1) ((exprMap f) e2) (mapMaybeExpr (exprMap f) e3))
  (Continue ) -> (Continue)
  (Break    ) -> (Break)
  (Return e1) -> (Return (mapMaybeExpr (exprMap f) e1))
  (Throw  e1) -> (Throw ((exprMap f) e1))
  (Match e1 cases (e2)) ->
    (Match
      ((exprMap f) e1)
      [ MatchCase
          { match_pattern = (exprMap f) $ match_pattern c
          , match_body    = (exprMap f) $ match_body c
          }
      | c <- cases
      ]
      (mapMaybeExpr (exprMap f) e2)
    )
  (InlineCall e1      ) -> (InlineCall ((exprMap f) e1))
  (Field       e1 lval) -> (Field ((exprMap f) e1) lval)
  (ArrayAccess e1 e2  ) -> (ArrayAccess ((exprMap f) e1) ((exprMap f) e2))
  (Call        e1 args) -> (Call ((exprMap f) e1) (map (exprMap f) args))
  (Cast        e1 t   ) -> (Cast ((exprMap f) e1) t)
  (TokenExpr    tc    ) -> (TokenExpr tc)
  (Unsafe       e1    ) -> (Unsafe ((exprMap f) e1))
  (BlockComment s     ) -> (BlockComment s)
  (New t args         ) -> (New t (map (exprMap f) args))
  (Copy   e1          ) -> (Copy ((exprMap f) e1))
  (Delete e1          ) -> (Delete ((exprMap f) e1))
  (Move   e1          ) -> (Move ((exprMap f) e1))
  (LexMacro     s  t  ) -> (LexMacro s t)
  (RangeLiteral e1 e2 ) -> (RangeLiteral ((exprMap f) e1) ((exprMap f) e2))
  (VectorLiteral items) -> (VectorLiteral (map (exprMap f) items))
  (VarDeclaration name t def) ->
    (VarDeclaration name t (mapMaybeExpr (exprMap f) def))

mapMaybeExpr f (Just ex) = Just (f ex)
mapMaybeExpr _ (Nothing) = Nothing
