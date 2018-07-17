module Kit.Ast.Binding where

import Kit.Ast.ConcreteType
import Kit.Ast.Declarations
import Kit.Ast.Expr
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Str

data BindingType
  = VarBinding ConcreteType
  | FunctionBinding ConcreteType [(Str, ConcreteType)] Bool
  | EnumConstructor TypePath [(Str, ConcreteType)]
  deriving (Eq, Show)

data Binding = Binding {
  bindingType :: BindingType,
  bindingNameMangling :: Maybe ModulePath
} deriving (Eq, Show)

newBinding b mangle = Binding {bindingType = b, bindingNameMangling = mangle}

data TypeBindingType a b
  = BindingType (TypeDefinition a b)
  | BindingTypedef
  | BindingTrait (TraitDefinition a b)

data TypeBinding = TypeBinding
  { typeBindingType :: TypeBindingType Expr (Maybe TypeSpec)
  , typeBindingConcrete :: ConcreteType
  }

newTypeBinding t c = TypeBinding {typeBindingType = t, typeBindingConcrete = c}
