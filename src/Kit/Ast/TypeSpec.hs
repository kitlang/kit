module Kit.Ast.TypeSpec where

import Data.Hashable
import Data.List
import Kit.Ast.BasicType
import Kit.Ast.ConcreteType
import Kit.Ast.ModulePath
import Kit.Ast.TypePath
import Kit.Ast.Value
import Kit.Parser.Span
import Kit.Str
import Kit.Str

{-
  A TypeSpec is a syntactic type as specified by a program. TypeSpecs will be
  resolved to a specific ConcreteType when expressions are typed.
-}
data TypeSpec
  = TypeSpec TypePath [TypeSpec] Span
  | ConstantTypeSpec (ValueLiteral (Maybe TypeSpec)) Span
  | TupleTypeSpec [TypeSpec] Span
  | FunctionTypeSpec TypeSpec [TypeSpec] Bool Span
  -- | TypeSpecConstant ValueLiteral
  {-
    This constructor can be used to force the TypeSpec to resolve to a specific
    ConcreteType without going through normal namespace resolution. This is
    used when we already know the underlying type when generating the AST,
    e.g. for C externs.
  -}
  | ConcreteType ConcreteType

makeTypeSpec s = TypeSpec ([], s) [] NoPos

typeSpecParams :: TypeSpec -> [TypeSpec]
typeSpecParams (TypeSpec _ params _     ) = params
typeSpecParams (FunctionTypeSpec _ _ _ _) = []
typeSpecParams _                          = []

typeSpecName :: TypeSpec -> Str
typeSpecName (TypeSpec (_, name) _ _                         ) = name
typeSpecName (FunctionTypeSpec (TypeSpec (_, name) _ _) _ _ _) = name

typeSpecPosition (TypeSpec _ _ pos          ) = pos
typeSpecPosition (FunctionTypeSpec t _ _ pos) = pos
typeSpecPosition (TupleTypeSpec _ pos       ) = pos
typeSpecPosition (ConcreteType _            ) = NoPos
typeSpecPosition (ConstantTypeSpec _ pos            ) = pos

instance Show TypeSpec where
  show (TypeSpec (tp) params _) = (s_unpack $ showTypePath tp) ++ (if params == [] then "" else "[" ++ (intercalate "," [show param | param <- params]) ++ "]")
  show (TupleTypeSpec t _) = "(" ++ intercalate ", " (map show t) ++ ")"
  show (FunctionTypeSpec t args var _) = "(" ++ intercalate ", " (map show args) ++ (if var then (if null args then "..." else ", ...") else "") ++ ") -> " ++ show t
  show (ConcreteType ct) = show ct

instance Eq TypeSpec where
  (==) (TypeSpec tp1 params1 _) (TypeSpec tp2 params2 _) = (tp1 == tp2) && (params1 == params2)
  (==) (TupleTypeSpec t1 _) (TupleTypeSpec t2 _) = (t1 == t2)
  (==) (FunctionTypeSpec tp1 params1 args1 v1) (FunctionTypeSpec tp2 params2 args2 v2) = (tp1 == tp2) && (params1 == params2) && (args1 == args2) && (v1 == v2)
  (==) (ConcreteType ct1) (ConcreteType ct2) = ct1 == ct2
  (==) a b = False

data TypeParam = TypeParam {
  paramName :: Str,
  constraints :: [TypeSpec]
} deriving (Eq, Show)

makeTypeParam s = TypeParam {paramName = s, constraints = []}
typeParamToSpec (TypeParam { paramName = s }) = makeTypeSpec s
