module Kit.Ast.Definitions.VarDefinition where

import Control.Monad
import Debug.Trace
import Kit.Ast.Definitions.Base
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypePath
import Kit.Ast.TypeSpec
import Kit.Parser.Span
import Kit.Str

data VarDefinition a b = VarDefinition {
  varName :: TypePath,
  varPos :: Span,
  varDoc :: Maybe Str,
  varMeta :: [Metadata],
  varModifiers :: [Modifier],
  varType :: b,
  varDefault :: Maybe a
} deriving (Eq, Show)

varRealName f = if hasMeta "extern" (varMeta f)
  then ([], tpName $ varName f)
  else varName f

newVarDefinition :: VarDefinition a b
newVarDefinition = VarDefinition
  { varName      = undefined
  , varDoc       = Nothing
  , varMeta      = []
  , varModifiers = [Public]
  , varType      = undefined
  , varDefault   = Nothing
  , varPos       = NoPos
  }

convertVarDefinition
  :: (Monad m)
  => Converter m a b c d
  -> VarDefinition a b
  -> m (VarDefinition c d)
convertVarDefinition (Converter { exprConverter = exprConverter, typeConverter = typeConverter }) v
  = do
    newType    <- typeConverter (varPos v) (varType v)
    newDefault <- maybeConvert exprConverter (varDefault v)
    return $ (newVarDefinition) { varName      = varName v
                                , varDoc       = varDoc v
                                , varMeta      = varMeta v
                                , varModifiers = varModifiers v
                                , varType      = newType
                                , varDefault   = newDefault
                                , varPos       = varPos v
                                }
