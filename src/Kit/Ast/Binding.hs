module Kit.Ast.Binding where

import Kit.Ast.ConcreteType
import Kit.Ast.ModulePath
import Kit.Str

data BindingType
  = VarBinding ConcreteType
  | FunctionBinding ConcreteType [(Str, ConcreteType)] Bool
  | EnumConstructor TypePath [(Str, ConcreteType)]
  deriving (Eq, Show)

data Binding = Binding { bindingType :: BindingType, bindingNameMangling :: Maybe ModulePath } deriving (Eq, Show)

newBinding b mangle = Binding {bindingType = b, bindingNameMangling = mangle}
