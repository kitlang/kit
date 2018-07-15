module Kit.Ast.Statement where

import Data.Traversable
import Kit.Ast.ConcreteType
import Kit.Ast.Declarations
import Kit.Ast.Expr
import Kit.Ast.ExprType
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.Operator
import Kit.Ast.TypeSpec
import Kit.Ast.Value
import Kit.Parser.Span
import Kit.Parser.Token
import Kit.Str

data Statement = Statement {stmt :: StatementType Expr (Maybe TypeSpec), stmtPos :: Span} deriving (Show)
instance Eq Statement where
  (==) a b = (stmt a) == (stmt b) && (stmtPos a == stmtPos b || stmtPos a == null_span || stmtPos b == null_span)

data StatementType a b
  = ModuleVarDeclaration (VarDefinition a b)
  | FunctionDeclaration (FunctionDefinition a b)
  | TypeDeclaration (TypeDefinition a b)
  | TraitDeclaration (TraitDefinition a b)
  | Implement (TraitImplementation a b)
  | Specialize TypeSpec TypeSpec
  | Typedef Str TypeSpec
  | Import ModulePath
  | Include FilePath
  deriving (Eq, Show)

makeStmt st = Statement {stmt = st, stmtPos = null_span}

ps :: Span -> StatementType Expr (Maybe TypeSpec) -> Statement
ps p st = Statement {stmt = st, stmtPos = p}
