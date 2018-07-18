module Kit.Ast.ConcreteType where

import Data.IORef
import Data.List
import Kit.Ast.BasicType
import Kit.Ast.ModulePath
import Kit.Parser.Span
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
  | TypeAnonStruct [(Str, ConcreteType)]
  | TypeEnum TypePath [ConcreteType]
  | TypeAnonEnum [Str]
  | TypeAbstract TypePath [ConcreteType]
  | TypeTypedef TypePath [ConcreteType]
  | TypeFunction ConcreteType ConcreteArgs Bool
  | TypeBasicType BasicType
  | TypePtr ConcreteType
  | TypeArr ConcreteType (Maybe Int)
  | TypeEnumConstructor TypePath ConcreteArgs
  | TypeIdentifier ConcreteType
  | TypeRange
  | TypeConstrained [(TypePath, [ConcreteType])]
  | TypeTypeVar TypeVar
  deriving (Eq)

instance Show ConcreteType where
  show (TypeAtom s) = "atom " ++ (s_unpack s)
  show (TypeStruct tp []) = "struct " ++ (s_unpack $ showTypePath tp)
  show (TypeStruct tp params) = "struct " ++ (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
  show (TypeAnonStruct f) = "(anon struct)"
  show (TypeEnum tp []) = "enum " ++ (s_unpack $ showTypePath tp)
  show (TypeEnum tp params) = "enum " ++ (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
  show (TypeAnonEnum variants) = "(anon enum {" ++ (intercalate ", " (map s_unpack variants)) ++ "})"
  show (TypeAbstract tp []) = "abstract " ++ (s_unpack $ showTypePath tp)
  show (TypeAbstract tp params) = "abstract " ++ (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
  show (TypeTypedef tp []) = (s_unpack $ showTypePath tp)
  show (TypeTypedef tp params) = (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
  show (TypeFunction rt args var) = "function (" ++ (intercalate ", " [s_unpack name ++ ": " ++ (show t) | (name, t) <- args]) ++ (if var then ", ..." else "") ++ "): (" ++ (show rt) ++ ")"
  show (TypeBasicType t) = show t
  show (TypePtr t) = "Ptr[" ++ (show t) ++ "]"
  show (TypeArr t (Just i)) = "Arr[" ++ (show t) ++ "] of length " ++ (show i)
  show (TypeArr t Nothing) = "Arr[" ++ (show t) ++ "]"
  show (TypeEnumConstructor tp _) = "enum " ++ (show tp) ++ " constructor"
  show (TypeIdentifier t) = "Identifier of " ++ (show t)
  show (TypeRange) = "range"
  show (TypeConstrained [(t, params)]) = "trait " ++ (s_unpack $ showTypePath t) ++ if null params then "" else ("[" ++ intercalate ", " (map show params) ++ "]")
  show (TypeConstrained ts) = "traits (" ++ (intercalate ", " [(s_unpack $ showTypePath tp) ++ if null params then "" else ("[" ++ intercalate ", " (map show params) ++ "]") | (tp, params) <- ts]) ++ ")"
  show (TypeTypeVar i) = show i

data TypeVar
  = TypeVar Int
  | TypeParamVar Str
  deriving (Eq)

instance Show TypeVar where
  show (TypeVar i) = "type var #" ++ (show i)
  show (TypeParamVar s) = "type param " ++ (s_unpack s)
