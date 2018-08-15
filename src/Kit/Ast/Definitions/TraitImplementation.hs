module Kit.Ast.Definitions.TraitImplementation where

import Control.Monad
import Kit.Ast.Definitions.Base
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
  implMod :: ModulePath,
  implMethods :: [FunctionDefinition a b],
  implDoc :: Maybe Str,
  implPos :: Span
} deriving (Eq, Show)

newTraitImplementation = TraitImplementation
  { implTrait   = undefined
  , implFor     = undefined
  , implMod     = undefined
  , implMethods = []
  , implDoc     = Nothing
  , implPos     = NoPos
  }

convertTraitImplementation
  :: (Monad m)
  => Converter m a b c d
  -> TraitImplementation a b
  -> m (TraitImplementation c d)
convertTraitImplementation converter@(Converter { exprConverter = exprConverter, typeConverter = typeConverter }) i
  = do
    trait   <- typeConverter (implPos i) (implTrait i)
    for     <- typeConverter (implPos i) (implFor i)
    methods <- forM (implMethods i)
                    (convertFunctionDefinition (\p -> converter))
    return $ (newTraitImplementation) { implTrait   = trait
                                      , implFor     = for
                                      , implMod     = implMod i
                                      , implMethods = methods
                                      , implDoc     = implDoc i
                                      , implPos     = implPos i
                                      }

vThisArgName :: Str
vThisArgName = "__vthis"
