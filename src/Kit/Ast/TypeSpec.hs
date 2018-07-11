module Kit.Ast.TypeSpec where

  import Data.Hashable
  import Data.List
  import Kit.Ast.BasicType
  import Kit.Ast.ConcreteType
  import Kit.Ast.ModulePath
  import Kit.Parser.Span
  import Kit.Str

  type TypeSpecArgs = [(Str, TypeSpec)]

  {-
    A TypeSpec is a syntactic type. TypeSpecs try to resolve to a specific
    ConcreteType when expressions are typed.
  -}
  data TypeSpec
    = TypeSpec TypePath [TypeParam] Span
    | TypeFunctionSpec TypeSpec [TypeParam] TypeSpecArgs Bool
    {-
      This variant can be used to force the BasicType to resolve to a specific
      ConcreteType without going through normal namespace resolution. This is
      used when we already know the underlying type when generating the AST,
      e.g. for C externs.
    -}
    | ConcreteType ConcreteType

  instance Show TypeSpec where
    show (TypeSpec (tp) params _) = (s_unpack $ showTypePath tp) ++ (if params == [] then "" else "[" ++ (intercalate "," [show param | param <- params]) ++ "]")

  instance Eq TypeSpec where
    (==) (TypeSpec tp1 params1 _) (TypeSpec tp2 params2 _) = (tp1 == tp2) && (params1 == params2)
    (==) (TypeFunctionSpec tp1 params1 args1 v1) (TypeFunctionSpec tp2 params2 args2 v2) = (tp1 == tp2) && (params1 == params2) && (args1 == args2) && (v1 == v2)
    (==) (ConcreteType ct1) (ConcreteType ct2) = ct1 == ct2
    (==) a b = False

  data TypeParam = TypeParam {
    param_type :: TypeSpec,
    constraints :: [TypeSpec]
  } deriving (Eq, Show)

  typeSpecPosition (TypeSpec _ _ pos) = pos
  typeSpecPosition (TypeFunctionSpec t _ _ _) = typeSpecPosition t
  typeSpecPosition (ConcreteType _) = null_span

  makeTypeSpec s = TypeSpec ([], s) [] null_span

  makeTypeParam t = TypeParam {param_type = t, constraints = []}
