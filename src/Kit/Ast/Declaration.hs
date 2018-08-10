module Kit.Ast.Declaration where

import Kit.Ast.Definitions
import Kit.Ast.UsingType
import Kit.Parser.Span

data Declaration a b
  = DeclVar (VarDefinition a b)
  | DeclFunction (FunctionDefinition a b)
  | DeclType (TypeDefinition a b)
  | DeclTrait (TraitDefinition a b)
  | DeclRuleSet (RuleSet a b)
  | DeclUsing (UsingType a b)
  deriving (Eq, Show)


declName (DeclVar v) = varName v
declName (DeclFunction v) = functionName v
declName (DeclType v) = typeName v
declName (DeclTrait v) = traitName v
declName (DeclRuleSet v) = ruleSetName v
declName (DeclUsing v) = "()"
