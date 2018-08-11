module Kit.Ast.Declaration where

import Kit.Ast.Definitions
import Kit.Ast.UsingType
import Kit.Parser.Span
import Kit.Str

data Declaration a b
  = DeclVar (VarDefinition a b)
  | DeclFunction (FunctionDefinition a b)
  | DeclType (TypeDefinition a b)
  | DeclTrait (TraitDefinition a b)
  | DeclRuleSet (RuleSet a b)
  | DeclUsing (UsingType a b)
  | DeclTuple b
  deriving (Eq, Show)


declName :: (Show b) => Declaration a b -> Str
declName (DeclVar v) = varName v
declName (DeclFunction v) = functionName v
declName (DeclType v) = typeName v
declName (DeclTrait v) = traitName v
declName (DeclRuleSet v) = ruleSetName v
declName (DeclTuple b) = s_pack $ show b
declName (DeclUsing v) = "()"
