module Kit.Ast.Declarations where

import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Str

data Definition a b
  = DefinitionVar (VarDefinition a b)
  | DefinitionFunction (FunctionDefinition a b)
  | DefinitionTrait (TraitDefinition a b)
  | DefinitionType (TypeDefinition a b)
  deriving (Eq, Show)

data VarDefinition a b = VarDefinition {
  varName :: Str,
  varDoc :: Maybe Str,
  varMeta :: [Metadata],
  varModifiers :: [Modifier],
  varType :: b,
  varDefault :: Maybe a,
  varNameMangling :: Maybe ModulePath
} deriving (Eq, Show)

newVarDefinition :: VarDefinition a b
newVarDefinition = VarDefinition
  { varName         = undefined
  , varDoc          = Nothing
  , varMeta         = []
  , varModifiers    = [Public]
  , varType         = undefined
  , varDefault      = Nothing
  , varNameMangling = Nothing
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
  functionNameMangling :: Maybe ModulePath
} deriving (Eq, Show)

newFunctionDefinition :: FunctionDefinition a b
newFunctionDefinition = FunctionDefinition
  { functionName         = undefined
  , functionDoc          = Nothing
  , functionMeta         = []
  , functionModifiers    = [Public]
  , functionParams       = []
  , functionArgs         = []
  , functionType         = undefined
  , functionBody         = Nothing
  , functionVarargs      = False
  , functionNameMangling = Nothing
  }

convertFunctionDefinition :: FunctionDefinition a b -> FunctionDefinition c d
convertFunctionDefinition f = (newFunctionDefinition)
  { functionName         = functionName f
  , functionDoc          = functionDoc f
  , functionMeta         = functionMeta f
  , functionModifiers    = functionModifiers f
  , functionParams       = functionParams f
  , functionVarargs      = functionVarargs f
  , functionNameMangling = functionNameMangling f
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
  implTrait :: b,
  implFor :: b,
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
  typeNameMangling :: Maybe ModulePath
} deriving (Eq, Show)

data TypeDefinitionType a b
  = Atom
  | Struct {structFields :: [VarDefinition a b]}
  | Enum {enumVariants :: [EnumVariant a b], enumUnderlyingType :: b}
  | Abstract {abstractUnderlyingType :: b}
  deriving (Eq, Show)

newTypeDefinition x = TypeDefinition
  { typeName         = x
  , typeDoc          = Nothing
  , typeMeta         = []
  , typeModifiers    = []
  , typeRules        = []
  , typeParams       = []
  , typeType         = undefined
  , typeNameMangling = Nothing
  }

convertTypeDefinition :: TypeDefinition a b -> TypeDefinition c d
convertTypeDefinition t = (newTypeDefinition (typeName t))
  { typeDoc          = typeDoc t
  , typeMeta         = typeMeta t
  , typeModifiers    = typeModifiers t
  , typeParams       = typeParams t
  , typeNameMangling = typeNameMangling t
  }

data RewriteRuleType a b
  = Rule (TermRewriteRule a b)
  | Method (FunctionDefinition a b)
  deriving (Eq, Show)

data TermRewriteRule a b = TermRewriteRule {
  ruleDoc :: Maybe Str,
  ruleMeta :: [Metadata],
  ruleModifiers :: [Modifier],
  ruleParams :: [TypeParam],
  ruleType :: b,
  rulePattern :: a,
  ruleBody :: Maybe a
} deriving (Eq, Show)
