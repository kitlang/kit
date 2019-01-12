module Kit.Compiler.Binding where

import Kit.Ast

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
  | ExprBinding a
  | ModuleBinding TypePath
  | TypedefBinding TypeSpec ModulePath Span
  deriving (Show, Eq)

bindingPos (VarBinding      x     ) = varPos x
bindingPos (FunctionBinding x     ) = functionPos x
bindingPos (TypeBinding     x     ) = typePos x
bindingPos (TraitBinding    x     ) = traitPos x
bindingPos (EnumConstructor x     ) = variantPos x
bindingPos (RuleSetBinding  x     ) = ruleSetPos x
bindingPos (ExprBinding     x     ) = position x
bindingPos (ModuleBinding   _     ) = NoPos
bindingPos (TypedefBinding _ _ pos) = pos

bindingIsPublic binding = case binding of
  VarBinding      v -> isPublic (varModifiers v)
  FunctionBinding f -> isPublic (functionModifiers f)
  _                 -> True

type SyntacticBinding = Binding Expr TypeSpec
type TypedBinding = Binding TypedExpr ConcreteType
