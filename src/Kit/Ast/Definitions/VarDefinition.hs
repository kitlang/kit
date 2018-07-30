module Kit.Ast.Definitions.VarDefinition where

import Control.Monad
import Kit.Ast.Definitions.Base
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Parser.Span
import Kit.Str

data VarDefinition a b = VarDefinition {
  varName :: Str,
  varPos :: Span,
  varDoc :: Maybe Str,
  varMeta :: [Metadata],
  varModifiers :: [Modifier],
  varType :: b,
  varDefault :: Maybe a,
  varNamespace :: [Str]
} deriving (Eq, Show)

newVarDefinition :: VarDefinition a b
newVarDefinition = VarDefinition
  { varName      = undefined
  , varDoc       = Nothing
  , varMeta      = []
  , varModifiers = [Public]
  , varType      = undefined
  , varDefault   = Nothing
  , varNamespace = []
  , varPos       = NoPos
  }

convertVarDefinition
  :: (Monad m)
  => (a -> m c)
  -> (b -> m d)
  -> VarDefinition a b
  -> m (VarDefinition c d)
convertVarDefinition exprConverter typeConverter v = do
  newType    <- typeConverter (varType v)
  newDefault <- maybeConvert exprConverter (varDefault v)
  return $ (newVarDefinition) { varName      = varName v
                              , varDoc       = varDoc v
                              , varMeta      = varMeta v
                              , varModifiers = varModifiers v
                              , varType      = newType
                              , varDefault   = newDefault
                              , varNamespace = varNamespace v
                              , varPos       = varPos v
                              }
