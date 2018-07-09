module Kit.Ast.BasicType where

  import Kit.Str

  {-
    A BasicType is a specific runtime type. BasicTypes are either C types or
    simple combinations of C types.
  -}
  data BasicType
    = CArray BasicType (Maybe Int)
    | CPtr BasicType
    | BasicTypeVoid
    | BasicTypeBool
    | BasicTypeInt Int
    | BasicTypeUint Int
    | BasicTypeFloat Int
    | BasicTypeStruct BasicStruct
    | BasicTypeVector BasicType (Maybe Int)
    | BasicTypeSimpleEnum Str [Str]
    | BasicTypeComplexEnum Str [BasicStruct]
    | BasicTypeAtom Str
    | BasicTypeFunction BasicType [BasicType] Bool
    -- If for some reason we can't parse type specifiers into a meaningful
    -- BasicType, the value isn't usable from Kit without casting.
    | BasicTypeUnknown
    deriving (Eq, Show)

  -- instance Show BasicType where

  -- (Name, [(Field Name, Field Type)])
  type BasicStruct = (Str, [(Str, BasicType)])
