{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Kit.Ast.BasicType where

import Data.Hashable
import Data.List
import GHC.Generics
import Kit.Ast.TypePath
import Kit.Str

{-
  A BasicType is a specific runtime type. BasicTypes are either C types or
  simple combinations of C types.
-}
data BasicType
  = CArray BasicType (Maybe Int)
  | CPtr BasicType
  | BasicTypeConst BasicType
  | BasicTypeCInt
  | BasicTypeCUint
  | BasicTypeCChar
  | BasicTypeCSize
  | BasicTypeVoid
  | BasicTypeBool
  | BasicTypeInt Int
  | BasicTypeUint Int
  | BasicTypeFloat Int
  -- Anonymous structs from headers are supported, but can't be defined in Kit.
  | BasicTypeStruct TypePath
  | BasicTypeAnonStruct (Maybe Str) BasicArgs
  | BasicTypeUnion TypePath
  | BasicTypeAnonUnion (Maybe Str) BasicArgs
  -- "Simple" enums have no additional data and can be represented as C enums.
  -- Simple enums can be anonymous if they were defined in a header.
  | BasicTypeSimpleEnum TypePath
  | BasicTypeAnonEnum (Maybe Str) [Str]
  -- "Complex" enums require defining an additional struct for each variant
  -- which contains data. The actual value type will be a struct of
  -- (discriminant, union of variant structs.)
  -- Complex enums can't be anonymous, since they can only come from Kit.
  | BasicTypeComplexEnum TypePath
  | BasicTypeFunction BasicType BasicArgs Bool
  | BasicTypeTuple Str [BasicType]
  | BasicTypeCFile
  -- If for some reason we can't parse type specifiers into a meaningful
  -- BasicType, the value isn't usable from Kit without casting.
  | BasicTypeUnknown
  deriving (Eq, Generic)

-- basicTypeAbbreviation (CArray t _       ) = "a" ++ basicTypeAbbreviation t
-- basicTypeAbbreviation (CPtr t           ) = "p" ++ basicTypeAbbreviation t
-- basicTypeAbbreviation (BasicTypeVoid    ) = "v"
-- basicTypeAbbreviation (BasicTypeBool    ) = "b"
-- basicTypeAbbreviation (BasicTypeCChar   ) = "c"
-- basicTypeAbbreviation (BasicTypeCInt    ) = "I"
-- basicTypeAbbreviation (BasicTypeCSize   ) = "z"
-- basicTypeAbbreviation (BasicTypeInt    i) = "i" ++ show i
-- basicTypeAbbreviation (BasicTypeUint   i) = "u" ++ show i
-- basicTypeAbbreviation (BasicTypeFloat  f) = "f" ++ show f
-- basicTypeAbbreviation (BasicTypeStruct x) = "s" ++ s_unpack x
-- basicTypeAbbreviation (BasicTypeAnonStruct args) =
--   "s"
--     ++ show (length args)
--     ++ (foldr (++) "" [ s_unpack n ++ basicTypeAbbreviation t | (n, t) <- args ]
--        )
-- basicTypeAbbreviation (BasicTypeUnion x) = "u" ++ s_unpack x
-- basicTypeAbbreviation (BasicTypeAnonUnion args) =
--   "u"
--     ++ show (length args)
--     ++ (foldr (++) "" [ s_unpack n ++ basicTypeAbbreviation t | (n, t) <- args ]
--        )
-- basicTypeAbbreviation (BasicTypeSimpleEnum  n) = "e" ++ s_unpack n
-- basicTypeAbbreviation (BasicTypeAnonEnum    _) = "e_" -- TODO
-- basicTypeAbbreviation (BasicTypeComplexEnum n) = "E" ++ s_unpack n
-- basicTypeAbbreviation (BasicTypeFunction rt args v) =
--   "f"
--     ++ show (length args)
--     ++ (foldr (++) "" [ s_unpack n ++ basicTypeAbbreviation t | (n, t) <- args ]
--        )
--     ++ basicTypeAbbreviation rt
-- basicTypeAbbreviation (BasicTypeTuple _ t) =
--   "t" ++ show (length t) ++ foldr (++) "" (map basicTypeAbbreviation t)
-- basicTypeAbbreviation (BasicTypeUnknown) = "q"

instance Hashable BasicType

instance Show BasicType where
  show (CArray t (Just i)) = show t ++ "[" ++ show i ++ "]"
  show (CArray t Nothing) = show t ++ "[]"
  show (CPtr BasicTypeCChar) = "CString"
  show (CPtr t) = "Ptr[" ++ show t ++ "]"
  show (BasicTypeConst t) = "Const[" ++ show t ++ "]"
  show (BasicTypeVoid) = "Void"
  show (BasicTypeBool) = "Bool"
  show (BasicTypeCChar) = "Char"
  show (BasicTypeCInt) = "Int"
  show (BasicTypeCSize) = "Size"
  show (BasicTypeInt 16) = "Short"
  show (BasicTypeInt 32) = "Int"
  show (BasicTypeInt 64) = "Long"
  show (BasicTypeInt w) = "Int" ++ show w
  show (BasicTypeUint 8) = "Byte"
  show (BasicTypeUint w) = "Uint" ++ show w
  show (BasicTypeFloat 32) = "Float"
  show (BasicTypeFloat 64) = "Double"
  show (BasicTypeFloat w) = "Float" ++ show w
  show (BasicTypeStruct name) = "struct " ++ (s_unpack $ showTypePath name)
  show (BasicTypeAnonStruct (Just x) _) = "(anon struct" ++ s_unpack x ++ ")"
  show (BasicTypeAnonStruct Nothing _) = "(anon struct)"
  show (BasicTypeUnion name) = "union " ++ (s_unpack $ showTypePath name)
  show (BasicTypeAnonUnion (Just x) _) = "(anon union" ++ s_unpack x ++ ")"
  show (BasicTypeAnonUnion Nothing _) = "(anon union)"
  show (BasicTypeSimpleEnum name) = "enum " ++ (s_unpack $ showTypePath name)
  show (BasicTypeAnonEnum (Just x) _) = "(anon enum " ++ s_unpack x ++ ")"
  show (BasicTypeAnonEnum Nothing _) = "(anon enum)"
  show (BasicTypeComplexEnum name) = "enum " ++ (s_unpack $ showTypePath name)
  show (BasicTypeFunction t args varargs) = "function (" ++ (intercalate ", " [s_unpack name ++ ": " ++ show argType | (name, argType) <- args]) ++ (if varargs then ", ..." else "") ++ "): " ++ show t
  show (BasicTypeTuple _ t) = "tuple (" ++ intercalate ", " (map show t) ++ ")"
  show (BasicTypeCFile) = "FILE"
  show (BasicTypeUnknown) = "???"

type BasicArgs = [(Str, BasicType)]

typeIsIntegral :: BasicType -> Bool
typeIsIntegral (BasicTypeInt  _)  = True
typeIsIntegral (BasicTypeUint _)  = True
typeIsIntegral BasicTypeCChar     = True
typeIsIntegral BasicTypeCInt      = True
typeIsIntegral BasicTypeCUint     = True
typeIsIntegral BasicTypeCSize     = True
typeIsIntegral (BasicTypeConst t) = typeIsIntegral t
typeIsIntegral _                  = False
