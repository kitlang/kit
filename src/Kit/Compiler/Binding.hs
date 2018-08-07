module Kit.Compiler.Binding where

import Kit.Ast
import Kit.Compiler.TypedExpr
import Kit.Parser.Span
import Kit.Str

{-
  Bindings are used as part of each Module's interface; they map names to type
  definitions, of which none of the types have been resolved yet. This allows
  other modules to look up the type and find a type variable to reference,
  which should eventually resolve to the correct type.

  Types in these bindings will be type variables; expressions are meaningless.
-}
data Binding = Binding {
  bindingType :: BindingType,
  bindingPath :: TypePath,
  bindingConcrete :: ConcreteType,
  bindingNamespace :: [Str],
  bindingPos :: Span
} deriving (Eq, Show)

data BindingType
  = VarBinding (VarDefinition TypedExpr ConcreteType)
  | FunctionBinding (FunctionDefinition TypedExpr ConcreteType)
  | TypeBinding (TypeDefinition TypedExpr ConcreteType)
  | TraitBinding (TraitDefinition TypedExpr ConcreteType)
  | EnumConstructor (EnumVariant TypedExpr ConcreteType)
  | RuleSetBinding (RuleSet Expr (Maybe TypeSpec))
  | TypedefBinding
  deriving (Show, Eq)

newBinding
  :: TypePath -> BindingType -> ConcreteType -> [Str] -> Span -> Binding
newBinding path b ct ns pos = Binding
  { bindingType      = b
  , bindingPath      = path
  , bindingConcrete  = ct
  , bindingNamespace = ns
  , bindingPos       = pos
  }
