module Kit.Ast.Definitions.EnumVariant where

import Control.Monad
import Kit.Ast.Definitions.Base
import Kit.Ast.Definitions.FunctionDefinition
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Str

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
