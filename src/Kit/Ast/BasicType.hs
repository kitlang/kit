module Kit.Ast.BasicType where

import Data.List
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
  | BasicTypeSimpleEnum Str [Str]
  | BasicTypeComplexEnum Str [BasicStruct]
  | BasicTypeAtom Str
  | BasicTypeFunction BasicType BasicArgs Bool
  -- If for some reason we can't parse type specifiers into a meaningful
  -- BasicType, the value isn't usable from Kit without casting.
  | BasicTypeUnknown
  deriving (Eq)

instance Show BasicType where
  show (CArray t (Just i)) = show t ++ "[" ++ show i ++ "]"
  show (CPtr t) = "Ptr[" ++ show t ++ "]"
  show (BasicTypeVoid) = "Void"
  show (BasicTypeBool) = "Bool"
  show (BasicTypeInt w) = "Int" ++ show w
  show (BasicTypeUint w) = "Uint" ++ show w
  show (BasicTypeFloat w) = "Float" ++ show w
  show (BasicTypeStruct (name, _)) = "struct " ++ s_unpack name
  show (BasicTypeSimpleEnum name _) = "enum " ++ s_unpack name
  show (BasicTypeComplexEnum name _) = "enum " ++ s_unpack name
  show (BasicTypeAtom s) = "atom " ++ s_unpack s
  show (BasicTypeFunction t args varargs) = "function (" ++ (intercalate ", " [s_unpack name ++ ": " ++ show argType | (name, argType) <- args]) ++ (if varargs then ", ..." else "") ++ "): " ++ show t
  show (BasicTypeUnknown) = "???"

type BasicArgs = [(Str, BasicType)]

-- (Name, [(Field Name, Field Type)])
type BasicStruct = (Str, BasicArgs)
