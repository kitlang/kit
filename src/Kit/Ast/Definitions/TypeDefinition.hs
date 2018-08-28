module Kit.Ast.Definitions.TypeDefinition where

import Control.Monad
import Kit.Ast.Definitions.Base
import Kit.Ast.Definitions.EnumVariant
import Kit.Ast.Definitions.FunctionDefinition
import Kit.Ast.Definitions.RewriteRule
import Kit.Ast.Definitions.VarDefinition
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypePath
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

typeSubPath :: ModulePath -> TypeDefinition a b -> Str -> TypePath
typeSubPath mp def s = (mp ++ [typeName def], s)

data TypeDefinitionType a b
  = Atom
  | Struct {structFields :: [VarDefinition a b]}
  | Union {unionFields :: [VarDefinition a b]}
  | Enum {enumVariants :: [EnumVariant a b], enumUnderlyingType :: b}
  | Abstract {abstractUnderlyingType :: b}
  deriving (Eq, Show)

newTypeDefinition = TypeDefinition
  { typeName          = undefined
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

implicitifyInstanceMethods
  :: Str
  -> b
  -> (FunctionDefinition a b -> a -> a)
  -> TypeDefinition a b
  -> TypeDefinition a b
implicitifyInstanceMethods thisName thisType body def = def
  { typeMethods = [ implicitifyMethod thisName thisType body method
                  | method <- typeMethods def
                  ]
  }

convertTypeDefinition
  :: (Monad m)
  => ParameterizedConverter m a b c d
  -> ModulePath
  -> TypeDefinition a b
  -> m (TypeDefinition c d)
convertTypeDefinition paramConverter modPath t = do
  let params =
        [ typeSubPath modPath t $ paramName param | param <- typeParams t ]
  let
    (converter@(Converter { exprConverter = exprConverter, typeConverter = typeConverter }))
      = paramConverter params
  let methodParamConverter methodParams =
        paramConverter (methodParams ++ params)
  newType <- case typeSubtype t of
    Atom                        -> return Atom
    Struct { structFields = f } -> do
      fields <- forM f (convertVarDefinition converter)
      return $ Struct {structFields = fields}
    Union { unionFields = f } -> do
      fields <- forM f (convertVarDefinition converter)
      return $ Union {unionFields = fields}
    Enum { enumVariants = variants, enumUnderlyingType = t' } -> do
      variants       <- forM variants (convertEnumVariant converter)
      underlyingType <- typeConverter (typePos t) t'
      return
        $ Enum {enumVariants = variants, enumUnderlyingType = underlyingType}
    Abstract { abstractUnderlyingType = ut } -> do
      u <- typeConverter (typePos t) ut
      return $ Abstract {abstractUnderlyingType = u}

  staticFields  <- forM (typeStaticFields t) (convertVarDefinition converter)
  staticMethods <- forM
    (typeStaticMethods t)
    (\f -> convertFunctionDefinition methodParamConverter
                                     (modPath ++ [typeName t])
                                     f
    )
  instanceMethods <- forM
    (typeMethods t)
    (\f -> convertFunctionDefinition methodParamConverter
                                     (modPath ++ [typeName t])
                                     f
    )

  -- since they are untyped, rulesets will not be converted and will be lost
  return $ (newTypeDefinition) { typeName          = typeName t
                               , typeDoc           = typeDoc t
                               , typeMeta          = typeMeta t
                               , typeModifiers     = typeModifiers t
                               , typeParams        = typeParams t
                               , typeNamespace     = typeNamespace t
                               , typeSubtype       = newType
                               , typePos           = typePos t
                               , typeStaticFields  = staticFields
                               , typeStaticMethods = staticMethods
                               , typeMethods       = instanceMethods
                               }

enumIsSimple enum = all variantIsSimple $ enumVariants enum

typeRuleSet t = newRuleSet { ruleSetName  = typeName t
                           , ruleSetPos   = typePos t
                           , ruleSetRules = typeRules t
                           }

thisArgName :: Str
thisArgName = "__this"
thisPtrName :: Str
thisPtrName = "__thisPtr"
