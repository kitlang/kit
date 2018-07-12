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
    A TypeSpec is a syntactic type as specified by a program. TypeSpecs will be
    resolved to a specific ConcreteType when expressions are typed.
  -}
  data TypeSpec
    = TypeSpec TypePath [TypeSpec] Span
    | TypeFunctionSpec TypeSpec [TypeSpec] TypeSpecArgs Bool
    {-
      This variant can be used to force the BasicType to resolve to a specific
      ConcreteType without going through normal namespace resolution. This is
      used when we already know the underlying type when generating the AST,
      e.g. for C externs.
    -}
    | ConcreteType ConcreteType

  makeTypeSpec s = TypeSpec ([], s) [] null_span

  typeSpecParams :: TypeSpec -> [TypeSpec]
  typeSpecParams (TypeSpec _ params _) = params
  typeSpecParams (TypeFunctionSpec _ params _ _) = params
  typeSpecParams _ = []

  typeSpecName :: TypeSpec -> Str
  typeSpecName (TypeSpec (_, name) _ _) = name
  typeSpecName (TypeFunctionSpec (TypeSpec (_, name) _ _) _ _ _) = name

  typeSpecPosition (TypeSpec _ _ pos) = pos
  typeSpecPosition (TypeFunctionSpec t _ _ _) = typeSpecPosition t
  typeSpecPosition (ConcreteType _) = null_span

  instance Show TypeSpec where
    show (TypeSpec (tp) params _) = (s_unpack $ showTypePath tp) ++ (if params == [] then "" else "[" ++ (intercalate "," [show param | param <- params]) ++ "]")
    show (ConcreteType ct) = show ct

  instance Eq TypeSpec where
    (==) (TypeSpec tp1 params1 _) (TypeSpec tp2 params2 _) = (tp1 == tp2) && (params1 == params2)
    (==) (TypeFunctionSpec tp1 params1 args1 v1) (TypeFunctionSpec tp2 params2 args2 v2) = (tp1 == tp2) && (params1 == params2) && (args1 == args2) && (v1 == v2)
    (==) (ConcreteType ct1) (ConcreteType ct2) = ct1 == ct2
    (==) a b = False
