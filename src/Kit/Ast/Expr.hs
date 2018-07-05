module Kit.Ast.Expr where

  import Data.Traversable
  import Kit.Ast.Base
  import Kit.Ast.Modifier
  import Kit.Ast.Operator
  import Kit.Ast.TypeSpec
  import Kit.Ast.Value
  import Kit.Parser.Span
  import Kit.Parser.Token
  import Kit.Str

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

  meta s = Metadata {meta_name = s, meta_args = []}
  metaExtern = meta "extern"

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

  newTypeDefinition x = TypeDefinition {
    type_name = x,
    type_doc = Nothing,
    type_meta = [],
    type_modifiers = [],
    type_rules = [],
    type_params = [],
    type_type = undefined
  }

  data VarDefinition = VarDefinition {
    var_name :: Lvalue,
    var_doc :: Maybe Str,
    var_meta :: [Metadata],
    var_modifiers :: [Modifier],
    var_type :: Maybe TypeSpec,
    var_default :: Maybe Expr
  } deriving (Eq, Show)

  newVarDefinition = VarDefinition {
    var_name = undefined,
    var_doc = Nothing,
    var_meta = [],
    var_modifiers = [Public],
    var_type = Nothing,
    var_default = Nothing
  }

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

  exprMap f e@(Expr {expr = et}) = case et of
    Block children -> f $ e {expr = Block (map f children)}
    Meta m e -> f $ e {expr = Meta m (f e)}
    TypeAnnotation e t -> f $ e {expr = TypeAnnotation (f e) t}
    PreUnop op e -> f $ e {expr = PreUnop op (f e)}
    PostUnop op e -> f $ e {expr = PostUnop op (f e)}
    Binop op e1 e2 -> f $ e {expr = Binop op (f e1) (f e2)}
    For e1 e2 e3 -> f $ e {expr = For (f e1) (f e2) (f e3)}
    While e1 e2 -> f $ e {expr = While (f e1) (f e2)}
    If e1 e2 (Just e3) -> f $ e {expr = If (f e1) (f e2) (Just $ f e3)}
    If e1 e2 Nothing -> f $ e {expr = If (f e1) (f e2) Nothing}
    Return (Just e) -> f $ e {expr = Return (Just $ f e)}
    Throw e -> f $ e {expr = Throw (f e)}
    {-Match Expr [MatchCase] (Maybe Expr)
    InlineCall Expr
    Field Expr Lvalue
    ArrayAccess Expr Expr
    Call Expr [Expr]
    Cast Expr TypeSpec
    TokenExpr [TokenClass]
    Unsafe Expr
    BlockComment Str
    New TypeSpec [Expr]
    Copy Expr
    Delete Expr
    Move Expr
    LexMacro Str [TokenClass]
    RangeLiteral Expr Expr
    VectorLiteral [Expr]
    Import ModulePath
    Include FilePath
    VarDeclaration VarDefinition
    FunctionDeclaration FunctionDefinition
    TypeDeclaration TypeDefinition
    TraitDeclaration TraitDefinition
    Implement TraitImplementation-}
    _ -> f e
