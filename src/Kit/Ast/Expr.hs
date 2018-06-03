module Kit.Ast.Expr where

  import qualified Data.ByteString.Lazy.Char8 as B
  import Kit.Ast.Modifier
  import Kit.Ast.Operator
  import Kit.Ast.Span
  import Kit.Ast.Type
  import Kit.Ast.Value
  import Kit.Parser.Token

  type ModulePath = [B.ByteString]

  data Expr = Expr {expr :: ExprType, pos :: Span} deriving (Show)
  instance Eq Expr where
    (==) a b = (expr a) == (expr b)

  e :: ExprType -> Expr
  e et = Expr {expr = et, pos = null_span}

  data MatchCase = MatchCase {match_pattern :: Expr, match_body :: Expr} deriving (Eq, Show)
  data Metadata = Metadata {meta_name :: B.ByteString, meta_args :: [Expr]} deriving (Eq, Show)

  data ExprType
    = Block [Expr]
    | Meta Metadata Expr
    | Literal ValueLiteral
    | This
    | Self
    | Identifier B.ByteString
    | TypeConstructor B.ByteString
    | TypeAnnotation Expr TypeSpec
    | PreUnop Operator Expr
    | PostUnop Operator Expr
    | Binop Operator Expr Expr
    | For B.ByteString Expr Expr
    | While Expr Expr
    | If Expr Expr (Maybe Expr)
    | Continue
    | Break
    | Return Expr
    | Throw Expr
    | Match Expr [MatchCase] (Maybe Expr)
    | InlineCall Expr
    | Field Expr B.ByteString
    | ArrayAccess Expr Expr
    | Call Expr [Expr]
    | Cast Expr TypeSpec
    | TokenExpr TokenClass
    | Unsafe Expr
    | BlockComment B.ByteString
    | New TypeSpec [Expr]
    | LexMacro B.ByteString [TokenClass]
    | Var VarDefinition
    | RangeLiteral Expr Expr
    | VectorLiteral [Expr]
    | Import ModulePath
    | FunctionDeclaration FunctionDefinition
    | TypeDeclaration Structure
    | TraitDeclaration TraitDefinition
    | Implement TraitImplementation
    deriving (Eq, Show)

  data Structure = Structure {
    structure_name :: B.ByteString,
    structure_doc :: Maybe B.ByteString,
    structure_meta :: [Metadata],
    structure_modifiers :: [Modifier],
    structure_rules :: [RewriteRule],
    structure_type :: StructureType
  } deriving (Eq, Show)

  data StructureType
    = Atom
    | Struct {struct_params :: [TypeParam], struct_fields :: [VarDefinition]}
    | Enum {enum_params :: [TypeParam], enum_variants :: [EnumVariant]}
    | Abstract {abstract_params :: [TypeParam], abstract_underlying_type :: TypeSpec}
    deriving (Eq, Show)

  data VarDefinition = VarDefinition {
    var_name :: B.ByteString,
    var_doc :: Maybe B.ByteString,
    var_meta :: [Metadata],
    var_modifiers :: [Modifier],
    var_type :: Maybe TypeSpec,
    var_default :: Maybe Expr
  } deriving (Eq, Show)

  data EnumVariant = EnumVariant {
    variant_name :: B.ByteString,
    variant_doc :: Maybe B.ByteString,
    variant_meta :: [Metadata],
    variant_modifiers :: [Modifier],
    variant_args :: [ArgSpec]
  } deriving (Eq, Show)

  data FunctionDefinition = FunctionDefinition {
    function_name :: B.ByteString,
    function_doc :: Maybe B.ByteString,
    function_meta :: [Metadata],
    function_modifiers :: [Modifier],
    function_params :: [TypeParam],
    function_args :: [ArgSpec],
    function_type :: Maybe TypeSpec,
    function_body :: Maybe Expr
  } deriving (Eq, Show)

  data ArgSpec = ArgSpec {
    arg_name :: B.ByteString,
    arg_type :: Maybe TypeSpec,
    arg_default :: Maybe Expr
  } deriving (Eq, Show)

  data RewriteRule = RewriteRule {
    rule_doc :: Maybe B.ByteString,
    rule_meta :: [Metadata],
    rule_modifiers :: [Modifier],
    rule_params :: [TypeParam],
    rule_type :: Maybe TypeSpec,
    rule_pattern :: Expr,
    rule_body :: Maybe Expr
  } deriving (Eq, Show)

  data TraitDefinition = TraitDefinition {
    trait_name :: B.ByteString,
    trait_doc :: Maybe B.ByteString,
    trait_meta :: [Metadata],
    trait_modifiers :: [Modifier],
    trait_params :: [TypeParam],
    trait_rules :: [RewriteRule]
  } deriving (Eq, Show)

  data TraitImplementation = TraitImplementation {
    impl_trait :: TypeSpec,
    impl_for :: TypeSpec,
    impl_rules :: [RewriteRule],
    impl_doc :: Maybe B.ByteString
  } deriving (Eq, Show)
