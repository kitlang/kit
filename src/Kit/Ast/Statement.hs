module Kit.Ast.Statement where

import Data.Maybe
import Kit.Ast.ConcreteType
import Kit.Ast.Definitions
import Kit.Ast.DefStatement
import Kit.Ast.ModulePath
import Kit.Ast.TypePath
import Kit.Ast.TypeSpec
import Kit.Ast.UsingType
import Kit.Ast.Span
import Kit.Str

data ImportType = ImportSingle | ImportWildcard | ImportDoubleWildcard deriving (Eq, Show)

data Statement a b = Statement {stmt :: StatementType a b, stmtPos :: Span} deriving (Eq, Show)

data StatementType a b
  = VarDeclaration (VarDefinition a b)
  | FunctionDeclaration (FunctionDefinition a b)
  | TypeDeclaration (TypeDefinition a b)
  | TraitDeclaration (TraitDefinition a b)
  | ExtendDefinition b [DefStatement a b]
  | Implement (TraitImplementation a b)
  | TraitDefault TypeSpec TypeSpec
  | RuleSetDeclaration (RuleSet a b)
  | Typedef TypePath TypeSpec
  | Import ModulePath ImportType
  | Include FilePath (Maybe Str)
  | ModuleUsing (UsingType a b)
  | MacroDeclaration (FunctionDefinition a b)
  | MacroCall TypePath [a]
  | TupleDeclaration b
  | StaticInit a
  deriving (Eq, Show)

makeStmt st = Statement {stmt = st, stmtPos = NoPos}

ps :: (Eq a, Eq b) => Span -> StatementType a b -> Statement a b
ps p st = Statement {stmt = st, stmtPos = p}

varDecl v = ps (varPos v) $ VarDeclaration v
functionDecl v = ps (functionPos v) $ FunctionDeclaration v
typeDecl v = ps (typePos v) $ TypeDeclaration v
traitDecl v = ps (traitPos v) $ TraitDeclaration v
implDecl v = ps (implPos v) $ Implement v
