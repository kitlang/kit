module Kit.Ast.ConcreteType where

  import Kit.Ast.Base
  import Kit.Ast.BasicType
  import Kit.Str

  data ConcreteType
    = TypeAtom Str
    | TypeStruct TypePath
    | TypeEnum TypePath
    | TypeAbstract TypePath
    | TypeTypedef TypePath
    | TypeFunction ConcreteType [(Str, ConcreteType)] Bool
    | TypeBasicType BasicType
    | TypePtr ConcreteType
    | TypeArr ConcreteType (Maybe Int)
    | TypeTraitPointer TypePath
    deriving (Eq, Show)
