module Kit.Ast.Definitions.Base where

import Control.Monad
import Kit.Ast.Metadata
import Kit.Ast.Modifier
import Kit.Ast.ModulePath
import Kit.Ast.TypeSpec
import Kit.Str

maybeConvert :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
maybeConvert converter val = do
  case val of
    Just v -> do
      converted <- converter v
      return $ Just converted
    Nothing -> return Nothing
