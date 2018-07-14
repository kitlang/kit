module Kit.Ast.Statement where

import Data.Traversable
import Kit.Ast.ConcreteType
import Kit.Ast.Expr
import Kit.Ast.ExprType
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.Operator
import Kit.Ast.TypeSpec
import Kit.Ast.Value
import Kit.Parser.Span
import Kit.Parser.Token
import Kit.Str

data Statement = Statement {stmt :: StatementType Expr (Maybe TypeSpec), stmtPos :: Span} deriving (Show)
instance Eq Statement where
  (==) a b = (stmt a) == (stmt b) && (stmtPos a == stmtPos b || stmtPos a == null_span || stmtPos b == null_span)

data StatementType a b
  = ModuleVarDeclaration (VarDefinition a b)
  | FunctionDeclaration (FunctionDefinition a b)
  | TypeDeclaration (TypeDefinition a b)
  | TraitDeclaration (TraitDefinition a b)
  | Implement (TraitImplementation a b)
  | Specialize TypeSpec TypeSpec
  | Typedef Str TypeSpec
  | Import ModulePath
  | Include FilePath
  deriving (Eq, Show)

makeStmt st = Statement {stmt = st, stmtPos = null_span}

ps :: Span -> StatementType Expr (Maybe TypeSpec) -> Statement
ps p st = Statement {stmt = st, stmtPos = p}

type RewriteRule = RewriteRuleType Expr (Maybe TypeSpec)

data RewriteRuleType a b
  = Rule (TermRewriteRule a b)
  | Method (FunctionDefinition a b)
  deriving (Eq, Show)

data VarDefinition a b = VarDefinition {
  varName :: Str,
  varDoc :: Maybe Str,
  varMeta :: [Metadata],
  varModifiers :: [Modifier],
  varType :: b,
  varDefault :: Maybe a,
  varMangleName :: Bool
} deriving (Eq, Show)

newVarDefinition :: VarDefinition a b
newVarDefinition = VarDefinition
  { varName        = undefined
  , varDoc         = Nothing
  , varMeta        = []
  , varModifiers   = [Public]
  , varType        = undefined
  , varDefault     = Nothing
  , varMangleName = True
  }

data FunctionDefinition a b = FunctionDefinition {
  functionName :: Str,
  functionDoc :: Maybe Str,
  functionMeta :: [Metadata],
  functionModifiers :: [Modifier],
  functionParams :: [TypeParam],
  functionArgs :: [ArgSpec a b],
  functionType :: b,
  functionBody :: Maybe a,
  functionVarargs :: Bool,
  functionMangleName :: Bool
} deriving (Eq, Show)

newFunctionDefinition :: FunctionDefinition a b
newFunctionDefinition = FunctionDefinition
  { functionName        = undefined
  , functionDoc         = Nothing
  , functionMeta        = []
  , functionModifiers   = [Public]
  , functionParams      = []
  , functionArgs        = []
  , functionType        = undefined
  , functionBody        = Nothing
  , functionVarargs     = False
  , functionMangleName = True
  }

data ArgSpec a b = ArgSpec {
  argName :: Str,
  argType :: b,
  argDefault :: Maybe a
} deriving (Eq, Show)

newArgSpec =
  ArgSpec {argName = undefined, argType = Nothing, argDefault = Nothing}

data TraitDefinition a b = TraitDefinition {
  traitName :: Str,
  traitDoc :: Maybe Str,
  traitMeta :: [Metadata],
  traitModifiers :: [Modifier],
  traitParams :: [TypeParam],
  traitRules :: [RewriteRuleType a b]
} deriving (Eq, Show)

data TraitImplementation a b = TraitImplementation {
  implTrait :: TypeSpec,
  implFor :: TypeSpec,
  implRules :: [RewriteRuleType a b],
  implDoc :: Maybe Str
} deriving (Eq, Show)

data EnumVariant a b = EnumVariant {
  variantName :: Str,
  variantDoc :: Maybe Str,
  variantMeta :: [Metadata],
  variantModifiers :: [Modifier],
  variantArgs :: [ArgSpec a b],
  variantValue :: Maybe a
} deriving (Eq, Show)

newEnumVariant = EnumVariant
  { variantName      = undefined
  , variantDoc       = Nothing
  , variantMeta      = []
  , variantModifiers = []
  , variantArgs      = []
  , variantValue     = Nothing
  }

data TypeDefinition a b = TypeDefinition {
  typeName :: Str,
  typeDoc :: Maybe Str,
  typeMeta :: [Metadata],
  typeModifiers :: [Modifier],
  typeRules :: [RewriteRuleType a b],
  typeType :: TypeDefinitionType a b,
  typeParams :: [TypeParam],
  typeMangleName :: Bool
} deriving (Eq, Show)

data TypeDefinitionType a b
  = Atom
  | Struct {struct_fields :: [VarDefinition a b]}
  | Enum {enum_variants :: [EnumVariant a b], enum_underlying_type :: b}
  | Abstract {abstract_underlying_type :: b}
  deriving (Eq, Show)

newTypeDefinition x = TypeDefinition
  { typeName        = x
  , typeDoc         = Nothing
  , typeMeta        = []
  , typeModifiers   = []
  , typeRules       = []
  , typeParams      = []
  , typeType        = undefined
  , typeMangleName = True
  }

data TypeParam = TypeParam {
  paramName :: Str,
  constraints :: [TypeSpec]
} deriving (Eq, Show)

makeTypeParam s = TypeParam {paramName = s, constraints = []}
typeParamToSpec (TypeParam { paramName = s }) = makeTypeSpec s

data TermRewriteRule a b = TermRewriteRule {
  ruleDoc :: Maybe Str,
  ruleMeta :: [Metadata],
  ruleModifiers :: [Modifier],
  ruleParams :: [TypeParam],
  ruleType :: b,
  rulePattern :: Expr,
  ruleBody :: Maybe a
} deriving (Eq, Show)
