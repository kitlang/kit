module Kit.Ast.Statement where

  import Data.Traversable
  import Kit.Ast.Base
  import Kit.Ast.ConcreteType
  import Kit.Ast.Expr
  import Kit.Ast.Modifier
  import Kit.Ast.Operator
  import Kit.Ast.TypeSpec
  import Kit.Ast.Value
  import Kit.Parser.Span
  import Kit.Parser.Token
  import Kit.Str

  data Statement = Statement {stmt :: StatementType, stmtPos :: Span} deriving (Show)
  instance Eq Statement where
    (==) a b = (stmt a) == (stmt b) && (stmtPos a == stmtPos b || stmtPos a == null_span || stmtPos b == null_span)

  data StatementType
    = ModuleVarDeclaration (VarDefinition Expr)
    | FunctionDeclaration FunctionDefinition
    | TypeDeclaration TypeDefinition
    | TraitDeclaration TraitDefinition
    | Implement TraitImplementation
    | Import ModulePath
    | Include FilePath
    deriving (Eq, Show)

  makeStmt st = Statement {stmt = st, stmtPos = null_span}

  ps :: Span -> StatementType -> Statement
  ps p st = Statement {stmt = st, stmtPos = p}

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
