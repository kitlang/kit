module Kit.Ast.Definitions.EnumVariant (
  EnumVariant (..),
  newEnumVariant,
  convertEnumVariant,
  discriminantFieldName,
  variantFieldName,
  variantIsSimple
) where

import Control.Monad
import Kit.Ast.Definitions.ArgSpec
import Kit.Ast.Definitions.Base
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.TypePath
import Kit.Ast.Span
import Kit.Str

data EnumVariant a b = EnumVariant {
  variantName :: TypePath,
  variantParent :: TypePath,
  variantPos :: Span,
  variantMeta :: [Metadata],
  variantModifiers :: [Modifier],
  variantArgs :: [ArgSpec a b],
  variantValue :: Maybe a
} deriving (Eq, Show)

instance Positioned (EnumVariant a b) where
  position = variantPos

variantRealName v = if hasMeta "extern" (variantMeta v)
  then ([], tpName $ variantName v)
  else subPath (variantParent v) (tpName $ variantName v)

newEnumVariant = EnumVariant
  { variantName      = undefined
  , variantParent    = undefined
  , variantMeta      = []
  , variantModifiers = []
  , variantArgs      = []
  , variantValue     = Nothing
  , variantPos       = NoPos
  }

variantIsSimple = null . variantArgs

convertEnumVariant
  :: (Monad m) => Converter m a b c d -> EnumVariant a b -> m (EnumVariant c d)
convertEnumVariant converter@(Converter { exprConverter = exprConverter }) v =
  do
    newArgs  <- forM (variantArgs v) (convertArgSpec converter)
    newValue <- maybeConvert exprConverter (variantValue v)
    return $ newEnumVariant { variantName      = variantName v
                            , variantParent    = variantParent v
                            , variantMeta      = variantMeta v
                            , variantModifiers = variantModifiers v
                            , variantArgs      = newArgs
                            , variantValue     = newValue
                            , variantPos       = variantPos v
                            }

discriminantFieldName :: Str
discriminantFieldName = "__dsc"

variantFieldName :: Str
variantFieldName = "__var"
