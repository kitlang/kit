module Kit.Ast.Statement where

  import Data.Traversable
  import Kit.Ast.ConcreteType
  import Kit.Ast.Expr
  import Kit.Ast.Modifier
  import Kit.Ast.ModulePath
  import Kit.Ast.Operator
  import Kit.Ast.TypeSpec
  import Kit.Ast.Value
  import Kit.Parser.Span
  import Kit.Parser.Token
  import Kit.Str

  data Statement = Statement {stmt :: StatementType Expr, stmtPos :: Span} deriving (Show)
  instance Eq Statement where
    (==) a b = (stmt a) == (stmt b) && (stmtPos a == stmtPos b || stmtPos a == null_span || stmtPos b == null_span)

  data StatementType a
    = ModuleVarDeclaration (VarDefinition a)
    | FunctionDeclaration FunctionDefinition
    | TypeDeclaration TypeDefinition
    | Typedef Str TypeSpec
    | TraitDeclaration TraitDefinition
    | Implement TraitImplementation
    | Import ModulePath
    | Include FilePath
    deriving (Eq, Show)

  makeStmt st = Statement {stmt = st, stmtPos = null_span}

  ps :: Span -> StatementType Expr -> Statement
  ps p st = Statement {stmt = st, stmtPos = p}

  data RewriteRule = Rule TermRewriteRule | Method FunctionDefinition deriving (Eq, Show)


  data VarDefinition a = VarDefinition {
    var_name :: Str,
    var_doc :: Maybe Str,
    var_meta :: [Metadata],
    var_modifiers :: [Modifier],
    var_type :: Maybe TypeSpec,
    var_default :: Maybe a
  } deriving (Eq, Show)

  newVarDefinition :: (ExprWrapper a) => VarDefinition a
  newVarDefinition = VarDefinition {
    var_name = undefined,
    var_doc = Nothing,
    var_meta = [],
    var_modifiers = [Public],
    var_type = Nothing,
    var_default = Nothing
  }

  data FunctionDefinition = FunctionDefinition {
    function_name :: Str,
    function_doc :: Maybe Str,
    function_meta :: [Metadata],
    function_modifiers :: [Modifier],
    function_params :: [TypeParam],
    function_args :: [ArgSpec],
    function_type :: Maybe TypeSpec,
    function_body :: Maybe Expr,
    function_varargs :: Bool
  } deriving (Eq, Show)

  newFunctionDefinition = FunctionDefinition {
    function_name = undefined,
    function_doc = Nothing,
    function_meta = [],
    function_modifiers = [Public],
    function_params = [],
    function_args = [],
    function_type = Nothing,
    function_body = Nothing,
    function_varargs = False
  }

  data ArgSpec = ArgSpec {
    arg_name :: Str,
    arg_type :: Maybe TypeSpec,
    arg_default :: Maybe Expr
  } deriving (Eq, Show)

  newArgSpec = ArgSpec {
    arg_name = undefined,
    arg_type = Nothing,
    arg_default = Nothing
  }

  data TraitDefinition = TraitDefinition {
    trait_name :: Str,
    trait_doc :: Maybe Str,
    trait_meta :: [Metadata],
    trait_modifiers :: [Modifier],
    trait_params :: [TypeParam],
    trait_rules :: [RewriteRule]
  } deriving (Eq, Show)

  data TraitImplementation = TraitImplementation {
    impl_trait :: TypeSpec,
    impl_for :: TypeSpec,
    impl_rules :: [RewriteRule],
    impl_doc :: Maybe Str
  } deriving (Eq, Show)

  data EnumVariant = EnumVariant {
    variant_name :: Str,
    variant_doc :: Maybe Str,
    variant_meta :: [Metadata],
    variant_modifiers :: [Modifier],
    variant_args :: [ArgSpec],
    variant_value :: Maybe Expr
  } deriving (Eq, Show)

  newEnumVariant = EnumVariant {
    variant_name = undefined,
    variant_doc = Nothing,
    variant_meta = [],
    variant_modifiers = [],
    variant_args = [],
    variant_value = Nothing
  }

  data TypeDefinition = TypeDefinition {
    type_name :: Str,
    type_doc :: Maybe Str,
    type_meta :: [Metadata],
    type_modifiers :: [Modifier],
    type_rules :: [RewriteRule],
    type_type :: TypeDefinitionType,
    type_params :: [TypeParam]
  } deriving (Eq, Show)

  data TypeDefinitionType
    = Atom
    | Struct {struct_fields :: [VarDefinition Expr]}
    | Enum {enum_variants :: [EnumVariant], enum_underlying_type :: Maybe TypeSpec}
    | Abstract {abstract_underlying_type :: Maybe TypeSpec}
    deriving (Eq, Show)

  newTypeDefinition x = TypeDefinition {
    type_name = x,
    type_doc = Nothing,
    type_meta = [],
    type_modifiers = [],
    type_rules = [],
    type_params = [],
    type_type = undefined
  }

  data TypeParam = TypeParam {
    param_name :: Str,
    constraints :: [TypeSpec]
  } deriving (Eq, Show)

  makeTypeParam s = TypeParam {param_name = s, constraints = []}
  typeParamToSpec (TypeParam {param_name = s}) = makeTypeSpec s

  data TermRewriteRule = TermRewriteRule {
    rule_doc :: Maybe Str,
    rule_meta :: [Metadata],
    rule_modifiers :: [Modifier],
    rule_params :: [TypeParam],
    rule_type :: Maybe TypeSpec,
    rule_pattern :: Expr,
    rule_body :: Maybe Expr
  } deriving (Eq, Show)
