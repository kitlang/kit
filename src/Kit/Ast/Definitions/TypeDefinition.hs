module Kit.Ast.Definitions.TypeDefinition where

import Control.Monad
import Kit.Ast.Definitions.EnumVariant
import Kit.Ast.Definitions.RewriteRule
import Kit.Ast.Definitions.VarDefinition
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Str

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

convertTypeDefinition
  :: (Monad m)
  => (a -> m c)
  -> (b -> m d)
  -> TypeDefinition a b
  -> m (TypeDefinition c d)
convertTypeDefinition exprConverter typeConverter t = do
  newType <- case typeType t of
    Atom                        -> return Atom
    Struct { structFields = f } -> do
      fields <- forM f (convertVarDefinition exprConverter typeConverter)
      return $ Struct {structFields = fields}
    Enum{}     -> return Atom -- TODO
    Abstract{} -> return Atom -- TODO
  return $ (newTypeDefinition (typeName t)) { typeDoc          = typeDoc t
                                            , typeMeta         = typeMeta t
                                            , typeModifiers    = typeModifiers t
                                            , typeParams       = typeParams t
                                            , typeNameMangling = typeNameMangling
                                              t
                                            , typeType         = newType
                                            }
