module Kit.Ast.Type where

  import Kit.Str

  type TypePath = ([Str], Str)
  type ParameterizedType = (TypePath, [TypeParam])

  data TypeSpec
    = ParameterizedTypePath ParameterizedType
    | FunctionType [TypeSpec] TypeSpec
    deriving (Eq, Show)

  data TypeParam = TypeParam {
    t :: ParameterizedType,
    constraints :: [TypeSpec]
  } deriving (Eq, Show)

  data ConcreteType
    = BasicType BasicType
    | UserType ParameterizedType
    | ConcreteFunctionType [ConcreteType] TypeSpec
    | TypeVar TypeVarId
    deriving (Eq, Show)

  type TypeVarId = Int

  data BasicType
    = TypeVoid
    | TypeInt Int
    | TypeUint Int
    | TypeFloat Int
    | TypeStruct Str [(Str, BasicType)]
    | TypeVector BasicType (Maybe Int)
    | TypeAtom Str
    | TypeFile
    deriving (Eq, Show)

  get_basic_type :: String -> Maybe BasicType
  get_basic_type "Int" = get_basic_type "Int32"
  get_basic_type "Int8" = Just $ TypeInt 8
  get_basic_type "Int16" = Just $ TypeInt 16
  get_basic_type "Int32" = Just $ TypeInt 32
  get_basic_type "Int64" = Just $ TypeInt 64
  get_basic_type "Uint8" = Just $ TypeUint 8
  get_basic_type "Uint16" = Just $ TypeUint 16
  get_basic_type "Uint32" = Just $ TypeUint 32
  get_basic_type "Uint64" = Just $ TypeUint 64
  get_basic_type "Float32" = Just $ TypeFloat 32
  get_basic_type "Float64" = Just $ TypeFloat 64
  get_basic_type "File" = Just TypeFile
