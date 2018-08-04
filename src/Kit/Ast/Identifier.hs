module Kit.Ast.Identifier where

import Kit.Ast.TypeSpec
import Kit.Str

data Identifier b
  -- A variable name
  = Var Str
  -- A macro variable with optional type annotation:
  -- `$abc` or `${abc: Int}`
  | MacroVar Str b
  deriving (Eq)

instance Show (Identifier b) where
  show (Var s) = s_unpack s
  show (MacroVar s _) = "$" ++ s_unpack s

identifierName x = case x of
  Var s        -> s
  MacroVar s _ -> s

convertIdentifier :: (Monad m) => (b -> m d) -> Identifier b -> m (Identifier d)
convertIdentifier typeConverter id = case id of
  Var s        -> return $ Var s
  MacroVar s t -> do
    t <- typeConverter t
    return $ MacroVar s t
