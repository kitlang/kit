{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module Kit.Ast.Types where

import Control.Monad
import Data.Hashable
import Data.List
import GHC.Generics
import Kit.Ast.BasicType
import Kit.Ast.ModulePath
import Kit.Ast.Span
import Kit.Ast.TypePath
import Kit.Ast.Value
import Kit.Str

{-
  A TypeSpec is a syntactic type as specified by a program. TypeSpecs will be
  resolved to a specific ConcreteType when expressions are typed.
-}
data TypeSpec
  = TypeSpec TypePath [TypeSpec] Span
  | ConstantTypeSpec ValueLiteral Span
  | TupleTypeSpec [TypeSpec] Span
  | PointerTypeSpec TypeSpec Span
  | FunctionTypeSpec TypeSpec [TypeSpec] (Maybe Str) Span
  {-
    This constructor can be used to force the TypeSpec to resolve to a specific
    ConcreteType without going through normal namespace resolution. This is
    used when we already know the underlying type when generating the AST,
    e.g. for C externs.
  -}
  | ConcreteType ConcreteType
  deriving (Generic)

makeTypeSpec s = TypeSpec ([], s) [] NoPos

typeSpecParams :: TypeSpec -> [TypeSpec]
typeSpecParams (TypeSpec _ params _     ) = params
typeSpecParams (FunctionTypeSpec _ _ _ _) = []
typeSpecParams _                          = []

instance Positioned TypeSpec where
  position (TypeSpec _ _ pos          ) = pos
  position (PointerTypeSpec t pos     ) = pos
  position (FunctionTypeSpec t _ _ pos) = pos
  position (TupleTypeSpec _ pos       ) = pos
  position (ConcreteType _            ) = NoPos
  position (ConstantTypeSpec _ pos    ) = pos

instance Show TypeSpec where
  show (TypeSpec (tp) params _) = (s_unpack $ showTypePath tp) ++ (if params == [] then "" else "[" ++ (intercalate "," [show param | param <- params]) ++ "]")
  show (ConstantTypeSpec v _) = show v
  show (PointerTypeSpec t _) = "&" ++ show t
  show (TupleTypeSpec t _) = "(" ++ intercalate ", " (map show t) ++ ")"
  show (FunctionTypeSpec t args var _) = "(" ++ intercalate ", " (map show args) ++ (case var of {Just x -> ", " ++ s_unpack x ++ "..."; Nothing -> ""}) ++ ") -> " ++ show t
  show (ConcreteType ct) = show ct

instance Eq TypeSpec where
  (==) (TypeSpec tp1 params1 _) (TypeSpec tp2 params2 _) = (tp1 == tp2) && (params1 == params2)
  (==) (TupleTypeSpec t1 _) (TupleTypeSpec t2 _) = (t1 == t2)
  (==) (ConstantTypeSpec v1 _) (ConstantTypeSpec v2 _) = (v1 == v2)
  (==) (PointerTypeSpec t1 _) (PointerTypeSpec t2 _) = (t1 == t2)
  (==) (FunctionTypeSpec tp1 params1 args1 v1) (FunctionTypeSpec tp2 params2 args2 v2) = (tp1 == tp2) && (params1 == params2) && (args1 == args2) && (v1 == v2)
  (==) (ConcreteType ct1) (ConcreteType ct2) = ct1 == ct2
  (==) a b = False

instance Hashable TypeSpec

type ConcreteArgs = [(Str, ConcreteType)]
type TraitConstraint = (TypePath, [ConcreteType])

{-
  A ConcreteType is either a specific compile-time type, or something like a
  type variable or type parameter that resolves to one. Not all ConcreteTypes
  will exist at runtime; abstracts and ranges for example will disappear. The
  underlying BasicType will be the expression's runtime type.
-}
data ConcreteType
  = TypeInstance TypePath [ConcreteType]
  -- anonymous types have a Maybe Str name, which is populated if they were
  -- defined in a typedef; otherwise we have no way to reference the type
  | TypeAnonStruct (Maybe Str) [(Str, ConcreteType)]
  | TypeAnonUnion (Maybe Str) [(Str, ConcreteType)]
  | TypeAnonEnum (Maybe Str) [Str]
  | TypeTypedef Str
  | TypeFunction ConcreteType ConcreteArgs (Maybe Str) [ConcreteType]
  | TypeBasicType BasicType
  | TypePtr ConcreteType
  | TypeEnumConstructor TypePath TypePath ConcreteArgs [ConcreteType]
  | TypeRange
  | TypeTraitConstraint TraitConstraint
  | TypeTuple [ConcreteType]
  | TypeTypeOf TypePath [ConcreteType]
  | TypeTypeVar TypeVar
  | TypeTemplateVar [TypePath] Int Span
  | TypeTypeParam TypePath
  | TypeRuleSet TypePath
  | TypeSelf
  | ConstantType ValueLiteral
  | ModuleType TypePath
  | VarArgs
  | TypeAny Span
  | MethodTarget ConcreteType
  | UnresolvedType TypeSpec ModulePath
  deriving (Eq, Generic)

instance Hashable ConcreteType

pattern TypeBox :: TypePath -> [ConcreteType] -> ConcreteType
pattern TypeBox tp params = TypeInstance (["kit", "common"], "Box") [TypeTraitConstraint (tp, params)]
pattern TypeArray :: ConcreteType -> Int -> ConcreteType
pattern TypeArray t n = TypeInstance (["kit", "common"], "CArray") [t, ConstantType (IntValue n)]
pattern TypeConst :: ConcreteType -> ConcreteType
pattern TypeConst t = TypeInstance (["kit", "common"], "Const") [t]
pattern TypeBool :: ConcreteType
pattern TypeBool = TypeInstance (["kit", "common"], "Bool") []
pattern TypeChar :: ConcreteType
pattern TypeChar = TypeInstance (["kit", "numeric"], "Char") []
pattern TypeSize :: ConcreteType
pattern TypeSize = TypeInstance (["kit", "numeric"], "Size") []
pattern TypeInt :: Int -> ConcreteType
pattern TypeInt w = TypeInstance (["kit", "numeric"], "Int") [ConstantType (IntValue w)]
pattern TypeUint :: Int -> ConcreteType
pattern TypeUint w = TypeInstance (["kit", "numeric"], "Uint") [ConstantType (IntValue w)]
pattern TypeFloat :: Int -> ConcreteType
pattern TypeFloat w = TypeInstance (["kit", "numeric"], "Float") [ConstantType (IntValue w)]
pattern TypeVoid :: ConcreteType
pattern TypeVoid = TypeBasicType BasicTypeVoid
pattern TypeCString :: ConcreteType
pattern TypeCString = TypeInstance (["kit", "common"], "CString") []

isNumericType TypeChar = True
isNumericType TypeSize = True
isNumericType (TypeInt _) = True
isNumericType (TypeUint _) = True
isNumericType (TypeFloat _) = True
isNumericType _ = False

instance Show ConcreteType where
  show (TypePtr t) = "Ptr[" ++ show t ++ "]"
  show (TypeInstance tp params) = (s_unpack $ showTypePath tp) ++ showParams params
  show (TypeAnonStruct (Just x) f) = "struct typedef " ++ s_unpack x
  show (TypeAnonStruct _ f) = "(anon struct: " ++ intercalate ", " [s_unpack name | (name, _) <- f] ++ ")"
  show (TypeAnonEnum (Just x) variants) = "enum typedef " ++ s_unpack x
  show (TypeAnonEnum _ variants) = "(anon enum {" ++ (intercalate ", " (map s_unpack variants)) ++ "})"
  show (TypeAnonUnion (Just x) f) = "union typedef " ++ s_unpack x
  show (TypeAnonUnion _ f) = "(anon union)"
  show (TypeTypedef name) = "typedef " ++ s_unpack name
  show (TypeFunction rt args var params) = "function (" ++ (intercalate ", " [show t | (_, t) <- args]) ++ (case var of {Just x -> ", " ++ s_unpack x ++ "..."; Nothing -> ""}) ++ ") -> " ++ show rt
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

type TypeVar = Int

basicType = TypeBasicType

mapType
  :: (Monad m)
  => (ConcreteType -> m ConcreteType)
  -> ConcreteType
  -> m ConcreteType
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
  args' <- forM args $ \(n, t) -> do
    t' <- f t
    return (n, t')
  p' <- mapM f p
  f $ TypeFunction rt' args' varargs p'
mapType f (TypeEnumConstructor tp s args p) = do
  args' <- forM args $ \(n, t) -> do
    t' <- f t
    return (n, t')
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
  :: (ConcreteType -> b -> b)
  -> b
  -> ConcreteType
  -> b
foldType f v t@(TypeInstance tp p) = foldr f v (t : p)
foldType f v t@(TypeAnonStruct x fields) = foldr f v (t : (map snd fields))
foldType f v t@(TypeAnonUnion x fields) = foldr f v (t : (map snd fields))
foldType f v t@(TypeFunction rt args varargs p) = foldr f v (rt : (map snd args) ++ p)
foldType f v t@(TypeEnumConstructor tp s args p) = foldr f v (t : (map snd args) ++ p)
foldType f v t@(TypeTuple c) = foldr f v (t : c)
foldType f v t@(TypeTraitConstraint (tp, p)) = foldr f v (t : p)
foldType f v t@(MethodTarget t2) = foldr f v [t, t2]
foldType f v t = f t v

-- FIXME: name...
mapType_ :: (ConcreteType -> a) -> ConcreteType -> [a]
mapType_ f t =
  (f t)
    : (case t of
        TypePtr t -> mapType_ f t
        _         -> []
      )

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

isPtr (TypePtr _) = True
isPtr _           = False

isTypeVar (TypeTypeVar _) = True
isTypeVar _               = False

isTypeInstance (TypeInstance _ _) = True
isTypeInstance _ = False

typeUnresolved :: ConcreteType -> Bool
typeUnresolved t = foldType (\t b -> b || typeUnresolved_ t) False t
typeUnresolved_ (TypeTypeVar _) = True
typeUnresolved_ (TypeTypeParam _) = True
typeUnresolved_ _ = False
