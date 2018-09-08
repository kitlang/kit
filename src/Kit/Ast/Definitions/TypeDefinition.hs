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
import Kit.NameMangling
import Kit.Parser.Span
import Kit.Str

data TypeDefinition a b = TypeDefinition {
  typeName :: TypePath,
  typeBundle :: Maybe TypePath,
  typeMonomorph :: [b],
  typePos :: Span,
  typeDoc :: Maybe Str,
  typeMeta :: [Metadata],
  typeModifiers :: [Modifier],
  typeMethods :: [FunctionDefinition a b],
  typeStaticFields :: [VarDefinition a b],
  typeStaticMethods :: [FunctionDefinition a b],
  typeRules :: [RewriteRule a b],
  typeSubtype :: TypeDefinitionType a b,
  typeParams :: [TypeParam]
} deriving (Eq, Show)

typeSubPath :: TypeDefinition a b -> Str -> TypePath
typeSubPath def s = subPath (typeName def) s

typeRealName t = if hasMeta "extern" (typeMeta t)
  then ([], tpName $ typeName t)
  else monomorphName (typeName t) (typeMonomorph t)

data TypeDefinitionType a b
  = Atom
  | Struct {structFields :: [VarDefinition a b]}
  | Union {unionFields :: [VarDefinition a b]}
  | Enum {enumVariants :: [EnumVariant a b], enumUnderlyingType :: b}
  | Abstract {abstractUnderlyingType :: b}
  deriving (Eq, Show)

newTypeDefinition = TypeDefinition
  { typeName          = undefined
  , typeBundle        = Nothing
  , typeMonomorph     = []
  , typeDoc           = Nothing
  , typeMeta          = []
  , typeModifiers     = []
  , typeMethods       = []
  , typeStaticFields  = []
  , typeStaticMethods = []
  , typeRules         = []
  , typeParams        = []
  , typeSubtype       = undefined
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
  -> TypeDefinition a b
  -> m (TypeDefinition c d)
convertTypeDefinition paramConverter t = do
  let params = [ typeSubPath t $ paramName param | param <- typeParams t ]
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
      return $ Enum
        { enumVariants       = [ variant
                                   { variantName   = typeSubPath t
                                     $ tpName
                                     $ variantName variant
                                   , variantParent = typeName t
                                   }
                               | variant <- variants
                               ]
        , enumUnderlyingType = underlyingType
        }
    Abstract { abstractUnderlyingType = ut } -> do
      u <- typeConverter (typePos t) ut
      return $ Abstract {abstractUnderlyingType = u}

  staticFields <- forM
    (typeStaticFields t)
    (\v -> convertVarDefinition converter
      $ v { varName = typeSubPath t (tpName $ varName v) }
    )
  staticMethods <- forM
    (typeStaticMethods t)
    (\f -> convertFunctionDefinition methodParamConverter
      $ f { functionName = typeSubPath t (tpName $ functionName f) }
    )
  instanceMethods <- forM
    (typeMethods t)
    (\f -> convertFunctionDefinition methodParamConverter
      $ f { functionName = typeSubPath t (tpName $ functionName f) }
    )

  mono <- forM (typeMonomorph t) $ typeConverter (typePos t)

  -- since they are untyped, rulesets will not be converted and will be lost
  return $ (newTypeDefinition) { typeName          = typeName t
                               , typeMonomorph     = mono
                               , typeBundle        = typeBundle t
                               , typeDoc           = typeDoc t
                               , typeMeta          = typeMeta t
                               , typeModifiers     = typeModifiers t
                               , typeParams        = typeParams t
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

thisPtrName :: Str
thisPtrName = "__this"
