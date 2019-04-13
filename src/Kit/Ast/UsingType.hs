{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.UsingType where

import Data.Hashable
import GHC.Generics
import Kit.Ast.Definitions.Base
import Kit.Ast.Span

data UsingType a b
  = UsingRuleSet b
  | UsingImplicit a
  deriving (Eq, Generic)

instance (Show a, Show b) => Show (UsingType a b) where
  show (UsingRuleSet b) = "rules " ++ show b
  show (UsingImplicit x) = "implicit " ++ show x

instance (Hashable a, Hashable b) => Hashable (UsingType a b)

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
