module Kit.Ast.Definitions.EnumVariant where

import Control.Monad
import Kit.Ast.Definitions.Base
import Kit.Ast.Definitions.FunctionDefinition
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Parser.Span
import Kit.Str

data EnumVariant a b = EnumVariant {
  variantName :: Str,
  variantPos :: Span,
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
  , variantPos       = NoPos
  }

variantIsSimple = null . variantArgs

convertEnumVariant
  :: (Monad m)
  => Converter m a b c d
  -> EnumVariant a b
  -> m (EnumVariant c d)
convertEnumVariant converter@(Converter { exprConverter = exprConverter, typeConverter = typeConverter }) v = do
  newArgs  <- forM (variantArgs v) (convertArgSpec converter)
  newValue <- maybeConvert exprConverter (variantValue v)
  return $ newEnumVariant { variantName      = variantName v
                          , variantDoc       = variantDoc v
                          , variantMeta      = variantMeta v
                          , variantModifiers = variantModifiers v
                          , variantArgs      = newArgs
                          , variantValue     = newValue
                          , variantPos       = variantPos v
                          }

discriminantFieldName :: Str
discriminantFieldName = "__discriminant"

variantFieldName :: Str
variantFieldName = "__variant"

discriminantMemberName :: Str -> Str
discriminantMemberName discriminant = s_concat ["variant_", discriminant]
