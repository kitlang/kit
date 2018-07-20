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
  | BasicTypeStruct (Maybe Str) BasicArgs
  | BasicTypeSimpleEnum (Maybe Str) [Str]
  | BasicTypeComplexEnum Str [(Str, BasicArgs)]
  | BasicTypeAtom
  | BasicTypeFunction BasicType BasicArgs Bool
  -- If for some reason we can't parse type specifiers into a meaningful
  -- BasicType, the value isn't usable from Kit without casting.
  | BasicTypeUnknown
  deriving (Eq)

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
