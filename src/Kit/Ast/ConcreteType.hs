{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.ConcreteType where

import Data.Hashable
import Data.IORef
import Data.List
import GHC.Generics
import Kit.Ast.BasicType
import Kit.Ast.ModulePath
import Kit.Parser.Span
import Kit.Str

type ConcreteArgs = [(Str, ConcreteType)]
type TraitConstraint = (TypePath, [ConcreteType])

{-
  A ConcreteType is either a specific compile-time type, or something like a
  type variable or type parameter that resolves to one. Not all ConcreteTypes
  will exist at runtime; abstracts and ranges for example will disappear. The
  underlying BasicType will be the expression's runtime type.
-}
data ConcreteType
  = TypeAtom
  | TypeStruct TypePath [ConcreteType]
  | TypeAnonStruct [(Str, ConcreteType)]
  | TypeEnum TypePath [ConcreteType]
  | TypeAnonEnum [Str]
  | TypeUnion TypePath [ConcreteType]
  | TypeAnonUnion [(Str, ConcreteType)]
  | TypeAbstract TypePath [ConcreteType]
  | TypeTypedef TypePath [ConcreteType]
  | TypeFunction ConcreteType ConcreteArgs Bool
  | TypeBasicType BasicType
  | TypePtr ConcreteType
  | TypeArr ConcreteType (Maybe Int)
  | TypeEnumConstructor TypePath Str ConcreteArgs
  | TypeIdentifier ConcreteType
  | TypeRange
  | TypeTraitConstraint TraitConstraint
  | TypeTuple [ConcreteType]
  | TypeTypeOf TypePath
  | TypeTypeVar TypeVar
  deriving (Eq, Generic)

instance Hashable ConcreteType

instance Show ConcreteType where
  show (TypeAtom) = "atom"
  show (TypeStruct tp []) = "struct " ++ (s_unpack $ showTypePath tp)
  show (TypeStruct tp params) = "struct " ++ (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
  show (TypeAnonStruct f) = "(anon struct)"
  show (TypeEnum tp []) = "enum " ++ (s_unpack $ showTypePath tp)
  show (TypeEnum tp params) = "enum " ++ (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
  show (TypeAnonEnum variants) = "(anon enum {" ++ (intercalate ", " (map s_unpack variants)) ++ "})"
  show (TypeUnion tp []) = "union " ++ (s_unpack $ showTypePath tp)
  show (TypeUnion tp params) = "union " ++ (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
  show (TypeAnonUnion f) = "(anon union)"
  show (TypeAbstract tp []) = "abstract " ++ (s_unpack $ showTypePath tp)
  show (TypeAbstract tp params) = "abstract " ++ (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
  show (TypeTypedef tp []) = (s_unpack $ showTypePath tp)
  show (TypeTypedef tp params) = (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
  show (TypeFunction rt args var) = "function (" ++ (intercalate ", " [s_unpack name ++ ": " ++ (show t) | (name, t) <- args]) ++ (if var then ", ..." else "") ++ ") -> " ++ show rt
  show (TypeBasicType t) = show t
  show (TypePtr (TypeBasicType (BasicTypeInt 8))) = "CString"
  show (TypePtr t) = "Ptr[" ++ (show t) ++ "]"
  show (TypeArr t (Just i)) = "Arr[" ++ (show t) ++ "] of length " ++ (show i)
  show (TypeArr t Nothing) = "Arr[" ++ (show t) ++ "]"
  show (TypeEnumConstructor tp d _) = "enum " ++ (show tp) ++ " constructor " ++ (s_unpack d)
  show (TypeIdentifier t) = "Identifier of " ++ (show t)
  show (TypeRange) = "range"
  show (TypeTraitConstraint (tp, params)) = "trait " ++ s_unpack (showTypePath tp)
  show (TypeTuple t) = "(" ++ intercalate ", " (map show t) ++ ")"
  show (TypeTypeOf t) = "typeof " ++ s_unpack (showTypePath t)
  show (TypeTypeVar i) = "type var #" ++ show i

type TypeVar = Int
