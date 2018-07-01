module Kit.Ast.Expr where

  import Kit.Str
  import Kit.Ast.Base
  import Kit.Ast.Modifier
  import Kit.Ast.Operator
  import Kit.Parser.Span
  import Kit.Ast.Type
  import Kit.Ast.Value
  import Kit.Parser.Token

  data Expr = Expr {expr :: ExprType, pos :: Span} deriving (Show)
  instance Eq Expr where
    (==) a b = (expr a) == (expr b) && (pos a == pos b || pos b == null_span)

  e :: ExprType -> Expr
  e et = ep et null_span

  ep :: ExprType -> Span -> Expr
  ep et p = Expr {expr = et, pos = p}

  pe :: Span -> ExprType -> Expr
  pe p et = ep et p

  me :: Span -> Expr -> Expr
  me p ex = Expr {expr = expr ex, pos = p}

  data MatchCase = MatchCase {match_pattern :: Expr, match_body :: Expr} deriving (Eq, Show)
  data Metadata = Metadata {meta_name :: Str, meta_args :: [Expr]} deriving (Eq, Show)

  data ExprType
    = Block [Expr]
    | Meta Metadata Expr
    | Literal ValueLiteral
    | This
    | Self
    | Lvalue Lvalue
    | TypeConstructor Str
    | TypeAnnotation Expr TypeSpec
    | PreUnop Operator Expr
    | PostUnop Operator Expr
    | Binop Operator Expr Expr
    | For Expr Expr Expr
    | While Expr Expr
    | If Expr Expr (Maybe Expr)
    | Continue
    | Break
    | Return (Maybe Expr)
    | Throw Expr
    | Match Expr [MatchCase] (Maybe Expr)
    | InlineCall Expr
    | Field Expr Lvalue
    | ArrayAccess Expr Expr
    | Call Expr [Expr]
    | Cast Expr TypeSpec
    | TokenExpr [TokenClass]
    | Unsafe Expr
    | BlockComment Str
    | New TypeSpec [Expr]
    | Copy Expr
    | Delete Expr
    | Move Expr
    | LexMacro Str [TokenClass]
    | RangeLiteral Expr Expr
    | VectorLiteral [Expr]
    | Import ModulePath
    | Include FilePath
    | VarDeclaration VarDefinition
    | FunctionDeclaration FunctionDefinition
    | TypeDeclaration TypeDefinition
    | TraitDeclaration TraitDefinition
    | Implement TraitImplementation
    deriving (Eq, Show)

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
    | Struct {struct_fields :: [VarDefinition]}
    | Enum {enum_variants :: [EnumVariant], enum_underlying_type :: Maybe TypeSpec}
    | Abstract {abstract_underlying_type :: Maybe TypeSpec}
    | Typedef {typedef_definition :: TypeSpec}
    deriving (Eq, Show)

  data VarDefinition = VarDefinition {
    var_name :: Lvalue,
    var_doc :: Maybe Str,
    var_meta :: [Metadata],
    var_modifiers :: [Modifier],
    var_type :: Maybe TypeSpec,
    var_default :: Maybe Expr
  } deriving (Eq, Show)

  data EnumVariant = EnumVariant {
    variant_name :: Str,
    variant_doc :: Maybe Str,
    variant_meta :: [Metadata],
    variant_modifiers :: [Modifier],
    variant_args :: [ArgSpec],
    variant_value :: Maybe Expr
  } deriving (Eq, Show)

  data FunctionDefinition = FunctionDefinition {
    function_name :: Str,
    function_doc :: Maybe Str,
    function_meta :: [Metadata],
    function_modifiers :: [Modifier],
    function_params :: [TypeParam],
    function_args :: [ArgSpec],
    function_type :: Maybe TypeSpec,
    function_body :: Maybe Expr
  } deriving (Eq, Show)

  data ArgSpec = ArgSpec {
    arg_name :: Str,
    arg_type :: Maybe TypeSpec,
    arg_default :: Maybe Expr
  } deriving (Eq, Show)

  data RewriteRule = Rule TermRewriteRule | Method FunctionDefinition deriving (Eq, Show)

  data TermRewriteRule = TermRewriteRule {
    rule_doc :: Maybe Str,
    rule_meta :: [Metadata],
    rule_modifiers :: [Modifier],
    rule_params :: [TypeParam],
    rule_type :: Maybe TypeSpec,
    rule_pattern :: Expr,
    rule_body :: Maybe Expr
  } deriving (Eq, Show)

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

  data Binding
    = VarBinding VarDefinition
    | FunctionBinding FunctionDefinition
    deriving (Eq, Show)

  -- TODO
  -- Apply a transform to an expression tree, returning the transformed
  -- expression.
  expr_apply :: (Expr -> Expr) -> Expr -> Expr
  expr_apply f expr = expr
