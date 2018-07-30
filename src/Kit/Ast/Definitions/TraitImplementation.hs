module Kit.Ast.Definitions.TraitImplementation where

import Control.Monad
import Kit.Ast.Definitions.FunctionDefinition
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Parser.Span
import Kit.Str

data TraitImplementation a b = TraitImplementation {
  implTrait :: b,
  implFor :: b,
  implMethods :: [FunctionDefinition a b],
  implDoc :: Maybe Str,
  implPos :: Span
} deriving (Eq, Show)
