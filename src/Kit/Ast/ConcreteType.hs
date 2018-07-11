module Kit.Ast.ConcreteType where

  import Data.IORef
  import Data.List
  import Kit.Ast.BasicType
  import Kit.Ast.ModulePath
  import Kit.Str

  type ConcreteArgs = [(Str, ConcreteType)]

  {-
    A ConcreteType is either a specific compile-time type, or something like a
    type variable or type parameter that resolves to one. Not all ConcreteTypes
    will exist at runtime; abstracts and ranges for example will disappear. The
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
    | TypeLvalue ConcreteType
    | TypeRange
    | TypeTraitPointer TypePath
    | TypeTypeVar TypeVar
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
    show (TypeLvalue t) = "lvalue of " ++ (show t)
    show (TypeRange) = "range"
    show (TypeTraitPointer tp) = "trait object of " ++ (s_unpack $ showTypePath tp)
    show (TypeTypeVar i) = show i

  data Binding
    = VarBinding ConcreteType
    | FunctionBinding ConcreteType [(Str, ConcreteType)] Bool
    deriving (Eq, Show)

  type EnumConstructor = (TypePath, ConcreteArgs)

  data TypeVar
    = TypeVar Int
    | TypeParamVar Str
    deriving (Eq)

  instance Show TypeVar where
    show (TypeVar i) = "type var #" ++ (show i)
    show (TypeParamVar s) = "type param " ++ (s_unpack s)


  typeNameToConcreteType :: Str -> Maybe ConcreteType
  typeNameToConcreteType "CString" = Just $ TypeBasicType $ CPtr $ BasicTypeInt 8
  typeNameToConcreteType "Bool" = Just $ TypeBasicType $ BasicTypeBool
  typeNameToConcreteType "Int8" = Just $ TypeBasicType $ BasicTypeInt 8
  typeNameToConcreteType "Int16" = Just $ TypeBasicType $ BasicTypeInt 16
  typeNameToConcreteType "Int32" = Just $ TypeBasicType $ BasicTypeInt 32
  typeNameToConcreteType "Int64" = Just $ TypeBasicType $ BasicTypeInt 64
  typeNameToConcreteType "Uint8" = Just $ TypeBasicType $ BasicTypeUint 8
  typeNameToConcreteType "Uint16" = Just $ TypeBasicType $ BasicTypeUint 16
  typeNameToConcreteType "Uint32" = Just $ TypeBasicType $ BasicTypeUint 32
  typeNameToConcreteType "Uint64" = Just $ TypeBasicType $ BasicTypeUint 64
  typeNameToConcreteType "Float32" = Just $ TypeBasicType $ BasicTypeFloat 32
  typeNameToConcreteType "Float64" = Just $ TypeBasicType $ BasicTypeFloat 64
  typeNameToConcreteType "Byte" = typeNameToConcreteType "Uint8"
  typeNameToConcreteType "Char" = typeNameToConcreteType "Int8"
  typeNameToConcreteType "Short" = typeNameToConcreteType "Int16"
  typeNameToConcreteType "Int" = typeNameToConcreteType "Int32"
  typeNameToConcreteType "Long" = typeNameToConcreteType "Int64"
  typeNameToConcreteType "Float" = typeNameToConcreteType "Float32"
  typeNameToConcreteType "Double" = typeNameToConcreteType "Float64"
  typeNameToConcreteType "Void" = Just $ TypeBasicType BasicTypeVoid
  typeNameToConcreteType _ = Nothing
