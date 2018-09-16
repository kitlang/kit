{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.ConcreteType where

import Control.Monad
import Data.Hashable
import Data.IORef
import Data.List
import GHC.Generics
import Kit.Ast.BasicType
import Kit.Ast.ModulePath
import Kit.Ast.TypePath
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
  | TypeInstance TypePath [ConcreteType]
  | TypeAnonStruct [(Str, ConcreteType)]
  | TypeAnonUnion [(Str, ConcreteType)]
  | TypeAnonEnum [Str]
  | TypeTypedef TypePath [ConcreteType]
  | TypeFunction ConcreteType ConcreteArgs Bool [ConcreteType]
  | TypeBasicType BasicType
  | TypePtr ConcreteType
  | TypeArray ConcreteType (Maybe Int)
  | TypeEnumConstructor TypePath TypePath ConcreteArgs [ConcreteType]
  | TypeRange
  | TypeTraitConstraint TraitConstraint
  | TypeTuple [ConcreteType]
  | TypeTypeOf TypePath
  | TypeTypeVar TypeVar
  | TypeTypeParam TypePath
  | TypeRuleSet TypePath
  | TypeBox TypePath [ConcreteType]
  | TypeSelf
  deriving (Eq, Generic)

instance Hashable ConcreteType

instance Show ConcreteType where
  show (TypeAtom) = "atom"
  show (TypeInstance tp params) = "instance " ++ (s_unpack $ showTypePath tp) ++ showParams params
  show (TypeAnonStruct f) = "(anon struct)"
  show (TypeAnonEnum variants) = "(anon enum {" ++ (intercalate ", " (map s_unpack variants)) ++ "})"
  show (TypeAnonUnion f) = "(anon union)"
  show (TypeTypedef tp []) = "typedef " ++ (s_unpack $ showTypePath tp)
  show (TypeTypedef tp params) = "typedef " ++ (s_unpack $ showTypePath tp) ++ showParams params
  show (TypeFunction rt args var params) = "function (" ++ (intercalate ", " [show t | (_, t) <- args]) ++ (if var then ", ..." else "") ++ ") -> " ++ show rt
  show (TypeBasicType t) = show t
  show (TypePtr (TypeBasicType (BasicTypeInt 8))) = "CString"
  show (TypePtr t) = "Ptr[" ++ (show t) ++ "]"
  show (TypeArray t (Just i)) = "CArray[" ++ (show t) ++ ", " ++ (show i) ++ "]"
  show (TypeArray t Nothing) = "CArray[" ++ (show t) ++ "]"
  show (TypeEnumConstructor tp d _ params) = "enum " ++ (s_unpack $ showTypePath tp) ++ " constructor " ++ (s_unpack $ showTypePath d) ++ "[" ++ (intercalate ", " (map show params)) ++ "]"
  show (TypeRange) = "range"
  show (TypeTraitConstraint (tp, params)) = "trait " ++ s_unpack (showTypePath tp) ++ showParams params
  show (TypeTuple t) = "(" ++ intercalate ", " (map show t) ++ ")"
  show (TypeTypeOf t) = "typeof " ++ s_unpack (showTypePath t)
  show (TypeTypeVar i) = "(unknown type #" ++ show i ++ ")"
  show (TypeTypeParam tp) = "type param " ++ s_unpack (showTypePath tp)
  show (TypeRuleSet tp) = "rules " ++ (s_unpack $ showTypePath tp)
  show (TypeBox tp params) = "Box[" ++ (s_unpack $ showTypePath tp) ++ showParams params ++ "]"
  show (TypeSelf) = "Self"

-- concreteTypeAbbreviation t = case t of
--   TypeAtom = "m"
--   TypeInstance (m,n) params = (intercalate "_" $ map s_unpack m) ++ n ++ (show $ length params) ++ (foldr (++) "" params)
--   TypeAnonStruct [(Str, ConcreteType)]
--   TypeAnonUnion [(Str, ConcreteType)]
--   TypeAnonEnum [Str]
--   TypeTypedef TypePath [ConcreteType]
--   TypeFunction ConcreteType ConcreteArgs Bool [ConcreteType]
--   TypeBasicType t = "b" ++ basicTypeAbbreviation t
--   TypePtr t = "p" ++ concreteTypeAbbreviation t
--   TypeArray t s = "a" ++ case s of {Just x -> show x; Nothing -> "_"} ++ concreteTypeAbbreviation t
--   TypeEnumConstructor TypePath Str ConcreteArgs [ConcreteType]
--   TypeRange
--   TypeTraitConstraint TraitConstraint
--   TypeTuple [ConcreteType]
--   TypeTypeOf TypePath
--   TypeTypeVar TypeVar
--   TypeTypeParam TypePath
--   TypeRuleSet TypePath
--   TypeBox TypePath [ConcreteType]
--   TypeSelf

showParams []     = ""
showParams params = "[" ++ (intercalate ", " (map show params)) ++ "]"

type TypeVar = Int

basicType = TypeBasicType
voidType = TypeBasicType BasicTypeVoid

mapType
  :: (Monad m)
  => (ConcreteType -> m ConcreteType)
  -> ConcreteType
  -> m ConcreteType
mapType f (TypeInstance tp p) = do
  p' <- mapM f p
  f $ TypeInstance tp p'
mapType f (TypeAnonStruct fields) = do
  fields' <- forM fields $ \(n, t) -> do
    t' <- f t
    return (n, t')
  f $ TypeAnonStruct fields'
mapType f (TypeAnonUnion fields) = do
  fields' <- forM fields $ \(n, t) -> do
    t' <- f t
    return (n, t')
  f $ TypeAnonUnion fields'
mapType f (TypeTypedef tp p) = do
  p' <- mapM f p
  f $ TypeTypedef tp p'
mapType f (TypeFunction rt args varargs p) = do
  rt'   <- f rt
  args' <- forM args $ \(n, t) -> do
    t' <- f t
    return (n, t')
  p' <- mapM f p
  f $ TypeFunction rt' args' varargs p'
mapType f (TypePtr t) = do
  t' <- f t
  f $ TypePtr t'
mapType f (TypeArray t l) = do
  t' <- f t
  f $ TypeArray t' l
mapType f (TypeEnumConstructor tp s args p) = do
  args' <- forM args $ \(n, t) -> do
    t' <- f t
    return (n, t')
  p' <- mapM f p
  f $ TypeEnumConstructor tp s args' p'
-- TypeTraitConstraint
mapType f (TypeBox tp p) = do
  p' <- mapM f p
  f $ TypeBox tp p'
mapType f (TypeTuple p) = do
  p' <- mapM f p
  f $ TypeTuple p'
mapType f (TypeTraitConstraint (tp, p)) = do
  p' <- mapM f p
  f $ TypeTraitConstraint (tp, p')
mapType f t = f t

substituteParams
  :: [(TypePath, ConcreteType)] -> ConcreteType -> IO ConcreteType
substituteParams params = mapType
  (\t -> case t of
    TypeTypeParam s -> substituteParam params s
    _               -> return t
  )

substituteParam :: [(TypePath, ConcreteType)] -> TypePath -> IO ConcreteType
substituteParam ((p, ct) : t) s =
  if p == s then return ct else substituteParam t s
substituteParam [] s = return $ TypeTypeParam s

-- = TypeAtom
-- | TypeInstance TypePath [ConcreteType]
-- | TypeAnonStruct [(Str, ConcreteType)]
-- | TypeAnonUnion [(Str, ConcreteType)]
-- | TypeAnonEnum [Str]
-- | TypeTypedef TypePath [ConcreteType]
-- | TypeFunction ConcreteType ConcreteArgs Bool
-- | TypeBasicType BasicType
-- | TypePtr ConcreteType
-- | TypeArray ConcreteType (Maybe Int)
-- | TypeEnumConstructor TypePath Str ConcreteArgs
-- | TypeRange
-- | TypeTraitConstraint TraitConstraint
-- | TypeTuple [ConcreteType]
-- | TypeTypeOf TypePath
-- | TypeTypeVar TypeVar
-- | TypeTypeParam Str
-- | TypeRuleSet TypePath
-- | TypeBox TypePath [ConcreteType]

isPtr (TypePtr _) = True
isPtr _           = False

isTypeVar (TypeTypeVar _) = True
isTypeVar _               = False
