module Kit.Ast.Declaration where

import Kit.Ast.Definitions
import Kit.Ast.Span
import Kit.Ast.TypePath
import Kit.Ast.Types
import Kit.Ast.UsingType
import Kit.Str

data Declaration a b
  = DeclVar (VarDefinition a b)
  | DeclFunction (FunctionDefinition a b)
  | DeclType (TypeDefinition a b)
  | DeclTrait (TraitDefinition a b)
  | DeclImpl (TraitImplementation a b)
  | DeclRuleSet (RuleSet a b)
  | DeclUsing (UsingType a b)
  | DeclTuple b
  | DeclTypedef TypePath TypeSpec Span
  deriving (Eq, Show)

declName :: (Show b) => Declaration a b -> TypePath
declName (DeclVar      v   ) = varName v
declName (DeclFunction v   ) = functionName v
declName (DeclType     v   ) = typeName v
declName (DeclTrait    v   ) = traitName v
declName (DeclImpl     v   ) = ([], "()")
declName (DeclRuleSet  v   ) = ruleSetName v
declName (DeclUsing    v   ) = ([], "()")
declName (DeclTuple    b   ) = ([], s_pack $ show b)
declName (DeclTypedef t _ _) = t
