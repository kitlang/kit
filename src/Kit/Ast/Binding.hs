module Kit.Ast.Binding where

import Kit.Ast.ConcreteType
import Kit.Ast.Definitions
import Kit.Ast.Expr
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Parser.Span
import Kit.Str

data Binding = Binding {
  bindingType :: BindingType,
  bindingPath :: TypePath,
  bindingConcrete :: ConcreteType,
  bindingNamespace :: [Str],
  bindingPos :: Span
} deriving (Eq, Show)

data BindingType
  = VarBinding
  | FunctionBinding
  | TypeBinding
  | TraitBinding
  | TypedefBinding
  | EnumConstructor
  deriving (Show, Eq)

newBinding path b ct ns pos = Binding
  { bindingType      = b
  , bindingPath      = path
  , bindingConcrete  = ct
  , bindingNamespace = ns
  , bindingPos       = pos
  }
