module Kit.Ast.Statement where

import Kit.Ast.Declaration
import Kit.Ast.Definitions
import Kit.Ast.Expr
import Kit.Ast.ModulePath
import Kit.Ast.Types
import Kit.Ast.UsingType
import Kit.Ast.Span
import Kit.Str

data Statement = Statement {stmt :: StatementType Expr (Maybe TypeSpec), stmtPos :: Span} deriving (Show)
instance Eq Statement where
  (==) a b = (stmt a) == (stmt b) && (stmtPos a == stmtPos b || stmtPos a == NoPos || stmtPos b == NoPos)

data StatementType a b
  = ModuleVarDeclaration (VarDefinition a b)
  | FunctionDeclaration (FunctionDefinition a b)
  | TypeDeclaration (TypeDefinition a b)
  | TraitDeclaration (TraitDefinition a b)
  | Implement (TraitImplementation a b)
  | Specialize TypeSpec TypeSpec
  | RuleSetDeclaration (RuleSet a b)
  | Typedef Str TypeSpec
  -- if the bool parameter is True, this is a wildcard import from a package
  | Import ModulePath Bool
  | Include FilePath (Maybe Str)
  | ModuleUsing (UsingType a b)
  deriving (Eq, Show)

makeStmt st = Statement {stmt = st, stmtPos = NoPos}

ps :: Span -> StatementType Expr (Maybe TypeSpec) -> Statement
ps p st = Statement {stmt = st, stmtPos = p}

type Decl = Declaration Expr (Maybe TypeSpec)
