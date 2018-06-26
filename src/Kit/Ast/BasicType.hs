module Kit.Ast.BasicType where

  import Kit.Str

  data BasicType
    = CString
    | CArray BasicType Int
    | CPtr BasicType
    | TypeVoid
    | TypeInt Int
    | TypeUint Int
    | TypeFloat Int
    | TypeStruct BasicStruct
    | TypeVector BasicType (Maybe Int)
    | TypeSimpleEnum Str [Str]
    | TypeComplexEnum Str [BasicStruct]
    | TypeAtom Str
    | TypeFile
    deriving (Eq, Show)

  -- (Name, [(Field Name, Field Type)])
  type BasicStruct = (Str, [(Str, BasicType)])

  get_basic_type :: String -> Maybe BasicType
  get_basic_type "CString" = Just $ CString
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
