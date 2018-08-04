module Kit.Ast.UsingType where

import Kit.Ast.TypePath

data UsingType a b
  = UsingRuleSet TypePath
  deriving (Eq, Show)

convertUsingType :: UsingType a b -> UsingType c d
convertUsingType u =
  case u of
    UsingRuleSet t -> UsingRuleSet t
