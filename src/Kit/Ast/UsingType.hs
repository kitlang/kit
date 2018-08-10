module Kit.Ast.UsingType where

import Kit.Ast.Definitions.Base
import Kit.Ast.TypePath
import Kit.Parser.Span
import Kit.Str

data UsingType a b
  = UsingRuleSet b
  | UsingImplicit a
  deriving (Eq)

instance (Show a, Show b) => Show (UsingType a b) where
  show (UsingRuleSet b) = "rules " ++ show b
  show (UsingImplicit x) = "implicit " ++ show x

convertUsingType
  :: (Monad m) => Converter m a b c d -> Span -> UsingType a b -> m (UsingType c d)
convertUsingType (Converter { exprConverter = exprConverter, typeConverter = typeConverter }) pos u
  = case u of
    UsingRuleSet  x -> do
      x' <- typeConverter pos x
      return $ UsingRuleSet x'
    UsingImplicit x -> do
      x' <- exprConverter x
      return $ UsingImplicit x'
