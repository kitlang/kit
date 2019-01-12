{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module Kit.Ast.ConcreteTypeBase where

import Control.Monad
import Data.Hashable
import Data.List
import GHC.Generics
import Kit.Ast.BasicType
import Kit.Ast.Definitions.ArgSpec
import Kit.Ast.ModulePath
import Kit.Ast.Span
import Kit.Ast.TypePath
import Kit.Ast.TypeSpecBase
import Kit.Ast.TypeVar
import Kit.Ast.Value
import Kit.Str

type TraitConstraintBase a = (TypePath, [ConcreteTypeBase a])

type ConcreteArgsBase a = [ArgSpec a (ConcreteTypeBase a)]

{-
  A ConcreteType is either a specific compile-time type, or something like a
  type variable or type parameter that resolves to one. Not all ConcreteTypes
  will exist at runtime; abstracts and ranges for example will disappear. The
  underlying BasicType will be the expression's runtime type.
-}
data ConcreteTypeBase a
  = TypeInstance TypePath [ConcreteTypeBase a]
  -- anonymous types have a Maybe Str name, which is populated if they were
  -- defined in a typedef; otherwise we have no way to reference the type
  | TypeAnonStruct (Maybe Str) [(Str, ConcreteTypeBase a)]
  | TypeAnonUnion (Maybe Str) [(Str, ConcreteTypeBase a)]
  | TypeAnonEnum (Maybe Str) [Str]
  | TypeTypedef Str
  | TypeFunction (ConcreteTypeBase a) (ConcreteArgsBase a) (Maybe Str) [ConcreteTypeBase a]
  | TypeBasicType BasicType
  | TypePtr (ConcreteTypeBase a)
  | TypeEnumConstructor TypePath TypePath (ConcreteArgsBase a) [ConcreteTypeBase a]
  | TypeRange
  | TypeTraitConstraint (TraitConstraintBase a)
  | TypeTuple [ConcreteTypeBase a]
  | TypeTypeOf TypePath [ConcreteTypeBase a]
  | TypeTypeVar TypeVar
  | TypeTemplateVar [TypePath] Int Span
  | TypeTypeParam TypePath
  | TypeRuleSet TypePath
  | TypeSelf
  | ConstantType ValueLiteral
  | ModuleType TypePath
  | VarArgs
  | TypeAny Span
  | MethodTarget (ConcreteTypeBase a)
  | UnresolvedType (TypeSpecBase (ConcreteTypeBase a)) ModulePath
  deriving (Eq, Generic)

instance (Hashable a) => Hashable (ConcreteTypeBase a)

pattern TypeBox :: TypePath -> [ConcreteTypeBase a] -> ConcreteTypeBase a
pattern TypeBox tp params = TypeInstance (["kit", "common"], "Box") [TypeTraitConstraint (tp, params)]
pattern TypeArray :: ConcreteTypeBase a -> Int -> ConcreteTypeBase a
pattern TypeArray t n = TypeInstance (["kit", "common"], "CArray") [t, ConstantType (IntValue n)]
pattern TypeConst :: ConcreteTypeBase a -> ConcreteTypeBase a
pattern TypeConst t = TypeInstance (["kit", "common"], "Const") [t]
pattern TypeBool :: ConcreteTypeBase a
pattern TypeBool = TypeInstance (["kit", "common"], "Bool") []
pattern TypeChar :: ConcreteTypeBase a
pattern TypeChar = TypeInstance (["kit", "numeric"], "Char") []
pattern TypeSize :: ConcreteTypeBase a
pattern TypeSize = TypeInstance (["kit", "numeric"], "Size") []
pattern TypeInt :: Int -> ConcreteTypeBase a
pattern TypeInt w = TypeInstance (["kit", "numeric"], "Int") [ConstantType (IntValue w)]
pattern TypeUint :: Int -> ConcreteTypeBase a
pattern TypeUint w = TypeInstance (["kit", "numeric"], "Uint") [ConstantType (IntValue w)]
pattern TypeFloat :: Int -> ConcreteTypeBase a
pattern TypeFloat w = TypeInstance (["kit", "numeric"], "Float") [ConstantType (IntValue w)]
pattern TypeVoid :: ConcreteTypeBase a
pattern TypeVoid = TypeBasicType BasicTypeVoid
pattern TypeCString :: ConcreteTypeBase a
pattern TypeCString = TypeInstance (["kit", "common"], "CString") []
pattern TypeVaList :: ConcreteTypeBase a
pattern TypeVaList = TypeBasicType (BasicTypeTypedef "__builtin_va_list")

isNumericType TypeChar = True
isNumericType TypeSize = True
isNumericType (TypeInt _) = True
isNumericType (TypeUint _) = True
isNumericType (TypeFloat _) = True
isNumericType _ = False

instance (Show a) => Show (ConcreteTypeBase a) where
  show (TypePtr t) = "Ptr[" ++ show t ++ "]"
  show (TypeInstance tp params) = (s_unpack $ showTypePath tp) ++ showParams params
  show (TypeAnonStruct (Just x) f) = "struct typedef " ++ s_unpack x
  show (TypeAnonStruct _ f) = "(anon struct: " ++ intercalate ", " [s_unpack name | (name, _) <- f] ++ ")"
  show (TypeAnonEnum (Just x) variants) = "enum typedef " ++ s_unpack x
  show (TypeAnonEnum _ variants) = "(anon enum {" ++ (intercalate ", " (map s_unpack variants)) ++ "})"
  show (TypeAnonUnion (Just x) f) = "union typedef " ++ s_unpack x
  show (TypeAnonUnion _ f) = "(anon union)"
  show (TypeTypedef name) = "typedef " ++ s_unpack name
  show (TypeFunction rt args var params) = "function (" ++ (intercalate ", " [show $ argType arg | arg <- args]) ++ (case var of {Just x -> ", " ++ s_unpack x ++ "..."; Nothing -> ""}) ++ ") -> " ++ show rt
  show (TypeBasicType t) = show t
  show (TypeEnumConstructor tp d _ params) = "enum " ++ (s_unpack $ showTypePath tp) ++ " constructor " ++ (s_unpack $ showTypePath d) ++ "[" ++ (intercalate ", " (map show params)) ++ "]"
  show (TypeRange) = "range"
  show (TypeTraitConstraint (tp, params)) = "trait " ++ s_unpack (showTypePath tp) ++ showParams params
  show (TypeTuple t) = "(" ++ intercalate ", " (map show t) ++ ")"
  show (TypeTypeOf t params) = "typeof " ++ s_unpack (showTypePath t) ++ showParams params
  show (TypeTypeVar i) = "(unknown type #" ++ show i ++ ")"
  show (TypeTemplateVar _ i _) = "(unknown template type #" ++ show i ++ ")"
  show (TypeTypeParam tp) = "type param " ++ s_unpack (showTypePath tp)
  show (TypeRuleSet tp) = "rules " ++ (s_unpack $ showTypePath tp)
  show (TypeSelf) = "Self"
  show (ConstantType v) = "$" ++ show v
  show (ModuleType tp) = "module " ++ s_unpack (showTypePath tp)
  show VarArgs = "..."
  show (TypeAny _) = "Any"
  show (MethodTarget t) = "this " ++ show t
  show (UnresolvedType t _) = show t

-- concreteTypeAbbreviation t = case t of
--   TypeInstance (m,n) params = (intercalate "_" $ map s_unpack m) ++ n ++ (show $ length params) ++ (foldr (++) "" params)
--   TypeAnonStruct [(Str, ConcreteType)]
--   TypeAnonUnion [(Str, ConcreteType)]
--   TypeAnonEnum [Str]
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

basicType = TypeBasicType

mapType
  :: (Monad m)
  => (ConcreteTypeBase a -> m (ConcreteTypeBase a))
  -> ConcreteTypeBase a
  -> m (ConcreteTypeBase a)
mapType f (TypeInstance tp p) = do
  p' <- mapM f p
  f $ TypeInstance tp p'
mapType f (TypeAnonStruct x fields) = do
  fields' <- forM fields $ \(n, t) -> do
    t' <- f t
    return (n, t')
  f $ TypeAnonStruct x fields'
mapType f (TypeAnonUnion x fields) = do
  fields' <- forM fields $ \(n, t) -> do
    t' <- f t
    return (n, t')
  f $ TypeAnonUnion x fields'
mapType f (TypeFunction rt args varargs p) = do
  rt'   <- f rt
  args' <- forM args $ \arg -> do
    t' <- f $ argType arg
    return $ arg {argType = t'}
  p' <- mapM f p
  f $ TypeFunction rt' args' varargs p'
mapType f (TypeEnumConstructor tp s args p) = do
  args' <- forM args $ \arg -> do
    t' <- f $ argType arg
    return $ arg {argType = t'}
  p' <- mapM f p
  f $ TypeEnumConstructor tp s args' p'
mapType f (TypeTuple p) = do
  p' <- mapM f p
  f $ TypeTuple p'
mapType f (TypeTraitConstraint (tp, p)) = do
  p' <- mapM f p
  f $ TypeTraitConstraint (tp, p')
mapType f (MethodTarget t) = do
  t <- mapType f t
  return $ MethodTarget t
mapType f t = f t

foldType
  :: (ConcreteTypeBase a -> b -> b)
  -> b
  -> ConcreteTypeBase a
  -> b
foldType f v t@(TypeInstance tp p) = foldr f v (t : p)
foldType f v t@(TypeAnonStruct x fields) = foldr f v (t : (map snd fields))
foldType f v t@(TypeAnonUnion x fields) = foldr f v (t : (map snd fields))
foldType f v t@(TypeFunction rt args varargs p) = foldr f v (rt : (map argType args) ++ p)
foldType f v t@(TypeEnumConstructor tp s args p) = foldr f v (t : (map argType args) ++ p)
foldType f v t@(TypeTuple c) = foldr f v (t : c)
foldType f v t@(TypeTraitConstraint (tp, p)) = foldr f v (t : p)
foldType f v t@(MethodTarget t2) = foldr f v [t, t2]
foldType f v t = f t v

-- FIXME: name...
mapType_ :: (ConcreteTypeBase a -> b) -> ConcreteTypeBase a -> [b]
mapType_ f t =
  (f t)
    : (case t of
        TypePtr t -> mapType_ f t
        _         -> []
      )

substituteParams
  :: [(TypePath, ConcreteTypeBase a)] -> ConcreteTypeBase a -> IO (ConcreteTypeBase a)
substituteParams params = mapType
  (\t -> case t of
    TypeTypeParam s -> substituteParam params s
    _               -> return t
  )

substituteParam :: [(TypePath, ConcreteTypeBase a)] -> TypePath -> IO (ConcreteTypeBase a)
substituteParam ((p, ct) : t) s =
  if p == s then return ct else substituteParam t s
substituteParam [] s = return $ TypeTypeParam s

isPtr (TypePtr _) = True
isPtr _           = False

isTypeVar (TypeTypeVar _) = True
isTypeVar _               = False

isTypeInstance (TypeInstance _ _) = True
isTypeInstance _ = False

typeUnresolved :: ConcreteTypeBase a -> Bool
typeUnresolved t = foldType (\t b -> b || typeUnresolved_ t) False t
typeUnresolved_ (TypeTypeVar _) = True
typeUnresolved_ (TypeTypeParam _) = True
typeUnresolved_ _ = False
