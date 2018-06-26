module Kit.Ast.Expr where

  import Kit.Str
  import Kit.Ast.Base
  import Kit.Ast.Modifier
  import Kit.Ast.Operator
  import Kit.Parser.Span
  import Kit.Ast.Type
  import Kit.Ast.Value
  import Kit.Parser.Token

  data Expr = Expr {expr :: ExprType, pos :: Span, expr_type :: Maybe ConcreteType} deriving (Show)
  instance Eq Expr where
    (==) a b = (expr a) == (expr b) && (pos a == pos b || pos b == null_span)

  e :: ExprType -> Expr
  e et = ep et null_span

  ep :: ExprType -> Span -> Expr
  ep et p = Expr {expr = et, pos = p, expr_type = Nothing}

  pe :: Span -> ExprType -> Expr
  pe p et = ep et p

  me :: Span -> Expr -> Expr
  me p ex = Expr {expr = expr ex, pos = p, expr_type = Nothing}

  te :: Expr -> ConcreteType -> Expr
  te ex t = ex {expr_type = Just t}

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
    | VarDef VarDefinition
    | RangeLiteral Expr Expr
    | VectorLiteral [Expr]
    | Import ModulePath
    | Include IncludePath
    | FunctionDeclaration FunctionDefinition
    | TypeDeclaration Structure
    | TraitDeclaration TraitDefinition
    | Implement TraitImplementation
    deriving (Eq, Show)

  data Structure = Structure {
    structure_name :: Str,
    structure_doc :: Maybe Str,
    structure_meta :: [Metadata],
    structure_modifiers :: [Modifier],
    structure_rules :: [RewriteRule],
    structure_type :: StructureType
  } deriving (Eq, Show)

  data StructureType
    = Atom
    | Struct {struct_params :: [TypeParam], struct_fields :: [VarDefinition]}
    | Enum {enum_params :: [TypeParam], enum_variants :: [EnumVariant], enum_underlying_type :: Maybe TypeSpec}
    | Abstract {abstract_params :: [TypeParam], abstract_underlying_type :: Maybe TypeSpec}
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

  data RewriteRule = Rule TermRewriteRule | Function FunctionDefinition deriving (Eq, Show)

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

  -- TODO
  -- Apply a transform to an expression tree, returning the transformed
  -- expression.
  expr_apply :: (Expr -> Expr) -> Expr -> Expr
  expr_apply f expr = expr
