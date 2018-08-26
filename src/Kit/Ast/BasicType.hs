{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

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
  -- In case we need "int" and not int16_t etc.
  | BasicTypeTrueInt
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
  | BasicTypeTuple Str [BasicType]
  -- If for some reason we can't parse type specifiers into a meaningful
  -- BasicType, the value isn't usable from Kit without casting.
  | BasicTypeUnknown
  deriving (Eq, Generic)

basicTypeAbbreviation (CArray t _) = "a" ++ basicTypeAbbreviation t
basicTypeAbbreviation (CPtr t) = "p" ++ basicTypeAbbreviation t
basicTypeAbbreviation (BasicTypeVoid) = "v"
basicTypeAbbreviation (BasicTypeBool) = "b"
basicTypeAbbreviation (BasicTypeInt i) = "i" ++ show i
basicTypeAbbreviation (BasicTypeUint i) = "u" ++ show i
basicTypeAbbreviation (BasicTypeFloat f) = "f" ++ show f
basicTypeAbbreviation (BasicTypeStruct (Just x) _) = "s" ++ s_unpack x
basicTypeAbbreviation (BasicTypeStruct Nothing args) = "s" ++ show (length args) ++ (foldr (++) "" [s_unpack n ++ basicTypeAbbreviation t | (n, t) <- args])
basicTypeAbbreviation (BasicTypeUnion (Just x) args) = "u" ++ s_unpack x
basicTypeAbbreviation (BasicTypeUnion Nothing args) = "u" ++ show (length args) ++ (foldr (++) "" [s_unpack n ++ basicTypeAbbreviation t | (n, t) <- args])
-- basicTypeAbbreviation (BasicTypeSimpleEnum n variants)
-- basicTypeAbbreviation (BasicTypeComplexEnum n args)
basicTypeAbbreviation (BasicTypeAtom) = "a"
-- basicTypeAbbreviation (BasicTypeFunction rt args v)
basicTypeAbbreviation (BasicTypeTuple _ t) = "t" ++ show (length t) ++ foldr (++) "" (map basicTypeAbbreviation t)
basicTypeAbbreviation (BasicTypeUnknown) = "q"

instance Hashable BasicType

instance Show BasicType where
  show (CArray t (Just i)) = show t ++ "[" ++ show i ++ "]"
  show (CArray t Nothing) = show t ++ "[]"
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
  show (BasicTypeUnion (Just name) _) = "union " ++ s_unpack name
  show (BasicTypeUnion Nothing _) = "(anon union)"
  show (BasicTypeSimpleEnum (Just name) _) = "enum " ++ s_unpack name
  show (BasicTypeSimpleEnum Nothing _) = "(anon enum)"
  show (BasicTypeComplexEnum name _) = "enum " ++ s_unpack name
  show (BasicTypeAtom) = "atom"
  show (BasicTypeFunction t args varargs) = "function (" ++ (intercalate ", " [s_unpack name ++ ": " ++ show argType | (name, argType) <- args]) ++ (if varargs then ", ..." else "") ++ "): " ++ show t
  show (BasicTypeTuple _ t) = "tuple (" ++ intercalate ", " (map show t) ++ ")"
  show (BasicTypeUnknown) = "???"

type BasicArgs = [(Str, BasicType)]
