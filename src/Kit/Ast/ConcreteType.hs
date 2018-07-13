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
  | TypeEnum TypePath [ConcreteType]
  | TypeAbstract TypePath [ConcreteType]
  | TypeTypedef TypePath [ConcreteType]
  | TypeFunction ConcreteType ConcreteArgs Bool
  | TypeBasicType BasicType
  | TypePtr ConcreteType
  | TypeArr ConcreteType (Maybe Int)
  | TypeEnumConstructor TypePath ConcreteArgs
  | TypeLvalue ConcreteType
  | TypeRange
  | TypeTrait TypePath
  | TypeTypeVar TypeVar
  deriving (Eq)

instance Show ConcreteType where
  show (TypeAtom s) = "atom " ++ (s_unpack s)
  show (TypeStruct tp []) = "struct " ++ (s_unpack $ showTypePath tp)
  show (TypeStruct tp params) = "struct " ++ (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
  show (TypeEnum tp []) = "enum " ++ (s_unpack $ showTypePath tp)
  show (TypeEnum tp params) = "enum " ++ (s_unpack $ showTypePath tp) ++ "[" ++ (intercalate ", " [show x | x <- params])
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
  show (TypeLvalue t) = "lvalue of " ++ (show t)
  show (TypeRange) = "range"
  show (TypeTrait tp) = "trait " ++ (s_unpack $ showTypePath tp)
  show (TypeTypeVar i) = show i

data Binding
  = VarBinding ConcreteType
  | FunctionBinding ConcreteType [(Str, ConcreteType)] Bool
  | EnumConstructor TypePath [(Str, ConcreteType)]
  deriving (Eq, Show)

data TypeVar
  = TypeVar Int
  | TypeParamVar Str
  deriving (Eq)

instance Show TypeVar where
  show (TypeVar i) = "type var #" ++ (show i)
  show (TypeParamVar s) = "type param " ++ (s_unpack s)
