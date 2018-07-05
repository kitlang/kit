module Kit.Ast.ConcreteType where

  import Kit.Ast.Base
  import Kit.Ast.BasicType
  import Kit.Str

  data ConcreteType
    = TypeAtom Str
    | TypeStruct ModulePath Str
    | TypeEnum ModulePath Str
    | TypeAbstract ModulePath Str
    | TypeTypedef ModulePath Str
    | TypeFunction ConcreteType [(Str, ConcreteType)] Bool
    | TypeUnknown TypeVar
    | TypeBasicType BasicType
    | TypePtr ConcreteType
    | TypeArr ConcreteType (Maybe Int)
    deriving (Eq, Show)

  type TypeVar = Int
