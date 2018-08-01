module Kit.Ast.Definitions.TypeDefinition where

import Control.Monad
import Kit.Ast.Definitions.EnumVariant
import Kit.Ast.Definitions.FunctionDefinition
import Kit.Ast.Definitions.RewriteRule
import Kit.Ast.Definitions.VarDefinition
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Parser.Span
import Kit.Str

data TypeDefinition a b = TypeDefinition {
  typeName :: Str,
  typePos :: Span,
  typeDoc :: Maybe Str,
  typeMeta :: [Metadata],
  typeModifiers :: [Modifier],
  typeMethods :: [FunctionDefinition a b],
  typeStaticFields :: [VarDefinition a b],
  typeStaticMethods :: [FunctionDefinition a b],
  typeRules :: [RewriteRule a b],
  typeSubtype :: TypeDefinitionType a b,
  typeParams :: [TypeParam],
  typeNamespace :: [Str]
} deriving (Eq, Show)

data TypeDefinitionType a b
  = Atom
  | Struct {structFields :: [VarDefinition a b]}
  | Union {unionFields :: [VarDefinition a b]}
  | Enum {enumVariants :: [EnumVariant a b], enumUnderlyingType :: b}
  | Abstract {abstractUnderlyingType :: b}
  deriving (Eq, Show)

newTypeDefinition x = TypeDefinition
  { typeName          = x
  , typeDoc           = Nothing
  , typeMeta          = []
  , typeModifiers     = []
  , typeMethods       = []
  , typeStaticFields  = []
  , typeStaticMethods = []
  , typeRules         = []
  , typeParams        = []
  , typeSubtype       = undefined
  , typeNamespace     = []
  , typePos           = NoPos
  }

convertTypeDefinition
  :: (Monad m)
  => (a -> m c)
  -> (b -> m d)
  -> TypeDefinition a b
  -> m (TypeDefinition c d)
convertTypeDefinition exprConverter typeConverter t = do
  newType <- case typeSubtype t of
    Atom                        -> return Atom
    Struct { structFields = f } -> do
      fields <- forM f (convertVarDefinition exprConverter typeConverter)
      return $ Struct {structFields = fields}
    Union { unionFields = f } -> do
      fields <- forM f (convertVarDefinition exprConverter typeConverter)
      return $ Union {unionFields = fields}
    Enum { enumVariants = variants, enumUnderlyingType = t } -> do
      variants <- forM variants (convertEnumVariant exprConverter typeConverter)
      underlyingType <- typeConverter t
      return
        $ Enum {enumVariants = variants, enumUnderlyingType = underlyingType}
    Abstract{} -> return Atom -- TODO
  -- TODO: static fields, methods, rules
  return $ (newTypeDefinition (typeName t)) { typeDoc       = typeDoc t
                                            , typeMeta      = typeMeta t
                                            , typeModifiers = typeModifiers t
                                            , typeParams    = typeParams t
                                            , typeNamespace = typeNamespace t
                                            , typeSubtype   = newType
                                            , typePos       = typePos t
                                            }

enumIsSimple enum = all variantIsSimple $ enumVariants enum

typeRuleSet t = newRuleSet { ruleSetName  = typeName t
                           , ruleSetPos   = typePos t
                           , ruleSetRules = typeRules t
                           }
