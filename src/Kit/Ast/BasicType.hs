{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.BasicType where

import Data.Hashable
import Data.List
import GHC.Generics
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
  -- Anonymous structs from headers are supported, but can't be defined in Kit.
  | BasicTypeStruct (Maybe Str) BasicArgs
  | BasicTypeUnion (Maybe Str) BasicArgs
  -- "Simple" enums have no additional data and can be represented as C enums.
  -- Simple enums can be anonymous if they were defined in a header.
  | BasicTypeSimpleEnum (Maybe Str) [Str]
  -- "Complex" enums require defining an additional struct for each variant
  -- which contains data. The actual value type will be a struct of
  -- (discriminant, union of variant structs.)
  -- Complex enums can't be anonymous, since they can only come from Kit.
  | BasicTypeComplexEnum Str [(Str, BasicArgs)]
  | BasicTypeAtom
  | BasicTypeFunction BasicType BasicArgs Bool
  -- If for some reason we can't parse type specifiers into a meaningful
  -- BasicType, the value isn't usable from Kit without casting.
  | BasicTypeUnknown
  deriving (Eq, Generic)

instance Hashable BasicType

instance Show BasicType where
  show (CArray t (Just i)) = show t ++ "[" ++ show i ++ "]"
  show (CPtr (BasicTypeInt 8)) = "CString"
  show (CPtr t) = "Ptr[" ++ show t ++ "]"
  show (BasicTypeVoid) = "Void"
  show (BasicTypeBool) = "Bool"
  show (BasicTypeInt 16) = "Short"
  show (BasicTypeInt 32) = "Int"
  show (BasicTypeInt 64) = "Long"
  show (BasicTypeInt w) = "Int" ++ show w
  show (BasicTypeUint 8) = "Byte"
  show (BasicTypeUint w) = "Uint" ++ show w
  show (BasicTypeFloat 32) = "Float"
  show (BasicTypeFloat 64) = "Double"
  show (BasicTypeFloat w) = "Float" ++ show w
  show (BasicTypeStruct (Just name) _) = "struct " ++ s_unpack name
  show (BasicTypeStruct Nothing _) = "(anon struct)"
  show (BasicTypeSimpleEnum (Just name) _) = "enum " ++ s_unpack name
  show (BasicTypeSimpleEnum Nothing _) = "(anon enum)"
  show (BasicTypeComplexEnum name _) = "enum " ++ s_unpack name
  show (BasicTypeAtom) = "atom"
  show (BasicTypeFunction t args varargs) = "function (" ++ (intercalate ", " [s_unpack name ++ ": " ++ show argType | (name, argType) <- args]) ++ (if varargs then ", ..." else "") ++ "): " ++ show t
  show (BasicTypeUnknown) = "???"

type BasicArgs = [(Str, BasicType)]
