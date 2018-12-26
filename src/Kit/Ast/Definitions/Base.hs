module Kit.Ast.Definitions.Base (
  Converter (..),
  ParameterizedConverter (..),
  converter,
  maybeConvert
) where

import Control.Monad
import Kit.Ast.TypePath
import Kit.Ast.Span

{-
  Converts AST structures from m X (a b) to m X (c d), where a/c are
  expressions and b/d are types.
-}
data Converter m a b c d = Converter {
  exprConverter :: (a -> m c),
  typeConverter :: (Span -> b -> m d)
}

{-
  Converter for things (functions, types) which need to care about type
  parameters when resolving types. Given a list of type parameter names, will
  return a Converter that will handle them in type lookups.
-}
type ParameterizedConverter m a b c d = [TypePath] -> m (Converter m a b c d)

converter e t = Converter {exprConverter = e, typeConverter = t}

maybeConvert :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
maybeConvert converter val = do
  case val of
    Just v -> do
      converted <- converter v
      return $ Just converted
    Nothing -> return Nothing
