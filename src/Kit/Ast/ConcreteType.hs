module Kit.Ast.ConcreteType where

  import Data.IORef
  import Data.List
  import Kit.Ast.BasicType
  import Kit.Ast.ModulePath
  import Kit.Str

  type TypeVar = Int
  type ConcreteArgs = [(Str, ConcreteType)]

  {-
    A ConcreteType is a specific compile-time type. Not all ConcreteTypes will
    exist at runtime; abstracts and ranges for example will disappear. The
    underlying BasicType will be the expression's runtime type.
  -}
  data ConcreteType
    = TypeAtom Str
    | TypeStruct TypePath [ConcreteType]
    | TypeEnum TypePath [ConcreteType]
    | TypeAbstract TypePath [ConcreteType]
    | TypeTypedef TypePath [ConcreteType]
    | TypeFunction ConcreteType ConcreteArgs Bool
    | TypeBasicType BasicType
    | TypePtr ConcreteType
    | TypeArr ConcreteType (Maybe Int)
    | TypeEnumConstructor TypePath ConcreteArgs
    | TypeLvalue
    | TypeRange
    | TypeTraitPointer TypePath
    | TypeVar TypeVar
    deriving (Eq)

  instance Show ConcreteType where
    show (TypeAtom s) = "atom " ++ (s_unpack s)
    show (TypeStruct tp []) = "struct " ++ (s_unpack $ showTypePath tp)
    show (TypeStruct tp params) = "struct " ++ (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
    show (TypeEnum tp []) = "enum " ++ (s_unpack $ showTypePath tp)
    show (TypeEnum tp params) = "enum " ++ (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
    show (TypeAbstract tp []) = "abstract " ++ (s_unpack $ showTypePath tp)
    show (TypeAbstract tp params) = "abstract " ++ (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
    show (TypeTypedef tp []) = "typedef " ++ (s_unpack $ showTypePath tp)
    show (TypeTypedef tp params) = "typedef " ++ (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
    show (TypeFunction rt args var) = "function (" ++ (intercalate ", " [s_unpack name ++ ": " ++ (show t) | (name, t) <- args]) ++ (if var then ", ..." else "") ++ "): (" ++ (show rt) ++ ")"
    show (TypeBasicType t) = show t
    show (TypePtr t) = "pointer to " ++ (show t)
    show (TypeArr t (Just i)) = "(" ++ (show t) ++ ")[" ++ (show i) ++ "]"
    show (TypeArr t Nothing) = "(" ++ (show t) ++ ")[]"
    show (TypeEnumConstructor tp _) = "enum " ++ (show tp) ++ " constructor"
    show (TypeLvalue) = "lvalue"
    show (TypeRange) = "range"
    show (TypeTraitPointer tp) = "trait object of " ++ (s_unpack $ showTypePath tp)
    show (TypeVar i) = "{{type var #" ++ (show i) ++ "}}"

  data Binding
    = VarBinding ConcreteType
    | FunctionBinding ConcreteType [(Str, ConcreteType)] Bool
    deriving (Eq, Show)

  type EnumConstructor = (TypePath, ConcreteArgs)
