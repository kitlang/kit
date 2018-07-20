module Kit.Ast.Binding where

import Kit.Ast.ConcreteType
import Kit.Ast.Declarations
import Kit.Ast.Expr
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Parser.Span
import Kit.Str

data Binding = Binding {
  bindingType :: BindingType,
  bindingConcrete :: ConcreteType,
  bindingNameMangling :: Maybe ModulePath,
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

newBinding b ct mangle pos =
  Binding {bindingType = b, bindingConcrete = ct, bindingNameMangling = mangle, bindingPos = pos}
