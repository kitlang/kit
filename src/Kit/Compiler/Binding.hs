module Kit.Compiler.Binding where

import Kit.Ast
import Kit.Compiler.TypedExpr

{-
  Bindings are used as part of each Module's interface; they map names to type
  definitions, of which none of the types have been resolved yet. This allows
  other modules to look up the type and find a type variable to reference,
  which should eventually resolve to the correct type.

  Types in these bindings will be type variables; expressions are meaningless.
-}
data Binding a b
  = VarBinding (VarDefinition a b)
  | FunctionBinding (FunctionDefinition a b)
  | TypeBinding (TypeDefinition a b)
  | TraitBinding (TraitDefinition a b)
  | EnumConstructor (EnumVariant a b)
  | RuleSetBinding (RuleSet a b)
  | TypedefBinding ConcreteType Span
  | ExprBinding a
  | ModuleBinding TypePath
  deriving (Show, Eq)

bindingPos (VarBinding      x   ) = varPos x
bindingPos (FunctionBinding x   ) = functionPos x
bindingPos (TypeBinding     x   ) = typePos x
bindingPos (TraitBinding    x   ) = traitPos x
bindingPos (EnumConstructor x   ) = variantPos x
bindingPos (RuleSetBinding  x   ) = ruleSetPos x
bindingPos (TypedefBinding _ pos) = pos
bindingPos (ExprBinding   x     ) = position x
bindingPos (ModuleBinding _     ) = NoPos

bindingIsPublic binding = case binding of
  VarBinding      v -> isPublic (varModifiers v)
  FunctionBinding f -> isPublic (functionModifiers f)
  _                 -> True

type SyntacticBinding = Binding Expr (Maybe TypeSpec)
type TypedBinding = Binding TypedExpr ConcreteType
