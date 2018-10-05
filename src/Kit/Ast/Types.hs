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
  | FunctionTypeSpec TypeSpec [TypeSpec] Bool Span
  -- | TypeSpecConstant ValueLiteral
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

typeSpecPosition (TypeSpec _ _ pos          ) = pos
typeSpecPosition (PointerTypeSpec t pos     ) = pos
typeSpecPosition (FunctionTypeSpec t _ _ pos) = pos
typeSpecPosition (TupleTypeSpec _ pos       ) = pos
typeSpecPosition (ConcreteType _            ) = NoPos
typeSpecPosition (ConstantTypeSpec _ pos    ) = pos

instance Show TypeSpec where
  show (TypeSpec (tp) params _) = (s_unpack $ showTypePath tp) ++ (if params == [] then "" else "[" ++ (intercalate "," [show param | param <- params]) ++ "]")
  show (PointerTypeSpec t _) = "&" ++ show t
  show (TupleTypeSpec t _) = "(" ++ intercalate ", " (map show t) ++ ")"
  show (FunctionTypeSpec t args var _) = "(" ++ intercalate ", " (map show args) ++ (if var then (if null args then "..." else ", ...") else "") ++ ") -> " ++ show t
  show (ConcreteType ct) = show ct

instance Eq TypeSpec where
  (==) (TypeSpec tp1 params1 _) (TypeSpec tp2 params2 _) = (tp1 == tp2) && (params1 == params2)
  (==) (TupleTypeSpec t1 _) (TupleTypeSpec t2 _) = (t1 == t2)
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
  = TypeAtom
  | TypeInstance TypePath [ConcreteType]
  | TypeAnonStruct [(Str, ConcreteType)]
  | TypeAnonUnion [(Str, ConcreteType)]
  | TypeAnonEnum [Str]
  | TypeTypedef Str
  | TypeFunction ConcreteType ConcreteArgs Bool [ConcreteType]
  | TypeBasicType BasicType
  | TypePtr ConcreteType
  | TypeEnumConstructor TypePath TypePath ConcreteArgs [ConcreteType]
  | TypeRange
  | TypeTraitConstraint TraitConstraint
  | TypeTuple [ConcreteType]
  | TypeTypeOf TypePath [ConcreteType]
  | TypeTypeVar TypeVar
  | TypeTypeParam TypePath
  | TypeRuleSet TypePath
  | TypeSelf
  | ConstantType ValueLiteral
  | UnresolvedType TypeSpec ModulePath
  deriving (Eq, Generic)

instance Hashable ConcreteType

pattern TypeBox :: TypePath -> [ConcreteType] -> ConcreteType
pattern TypeBox tp params = TypeInstance (["kit", "common"], "Box") [TypeTraitConstraint (tp, params)]
pattern TypeArray :: ConcreteType -> Int -> ConcreteType
pattern TypeArray t n = TypeInstance (["kit", "common"], "CArray") [t, ConstantType (IntValue n)]

instance Show ConcreteType where
  show (TypeAtom) = "atom"
  show (TypePtr t) = "Ptr[" ++ show t ++ "]"
  show (TypeInstance tp params) = (s_unpack $ showTypePath tp) ++ showParams params
  show (TypeAnonStruct f) = "(anon struct)"
  show (TypeAnonEnum variants) = "(anon enum {" ++ (intercalate ", " (map s_unpack variants)) ++ "})"
  show (TypeAnonUnion f) = "(anon union)"
  show (TypeTypedef name) = "typedef " ++ s_unpack name
  show (TypeFunction rt args var params) = "function (" ++ (intercalate ", " [show t | (_, t) <- args]) ++ (if var then ", ..." else "") ++ ") -> " ++ show rt
  show (TypeBasicType t) = show t
  show (TypeEnumConstructor tp d _ params) = "enum " ++ (s_unpack $ showTypePath tp) ++ " constructor " ++ (s_unpack $ showTypePath d) ++ "[" ++ (intercalate ", " (map show params)) ++ "]"
  show (TypeRange) = "range"
  show (TypeTraitConstraint (tp, params)) = "trait " ++ s_unpack (showTypePath tp) ++ showParams params
  show (TypeTuple t) = "(" ++ intercalate ", " (map show t) ++ ")"
  show (TypeTypeOf t params) = "typeof " ++ s_unpack (showTypePath t) ++ showParams params
  show (TypeTypeVar i) = "(unknown type #" ++ show i ++ ")"
  show (TypeTypeParam tp) = "type param " ++ s_unpack (showTypePath tp)
  show (TypeRuleSet tp) = "rules " ++ (s_unpack $ showTypePath tp)
  show (TypeSelf) = "Self"
  show (ConstantType v) = "$" ++ show v

-- concreteTypeAbbreviation t = case t of
--   TypeAtom = "m"
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
-- TypeTraitConstraint
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
