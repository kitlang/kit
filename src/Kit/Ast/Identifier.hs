{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.Identifier where

import Data.Hashable
import GHC.Generics
import Kit.Ast.TypePath
import Kit.Str

data Identifier b
  -- A variable name
  = Var TypePath
  -- A macro variable with optional type annotation:
  -- `$abc` or `${abc: Int}`
  | MacroVar Str b
  | Hole
  deriving (Eq, Generic)

instance Show (Identifier b) where
  show (Var s) = s_unpack $ showTypePath s
  show (MacroVar s _) = "$" ++ s_unpack s
  show Hole = "_"

instance (Hashable b) => Hashable (Identifier b)

convertIdentifier :: (Monad m) => (b -> m d) -> Identifier b -> m (Identifier d)
convertIdentifier typeConverter id = case id of
  Var s        -> return $ Var s
  MacroVar s t -> do
    t <- typeConverter t
    return $ MacroVar s t
  Hole -> return Hole
