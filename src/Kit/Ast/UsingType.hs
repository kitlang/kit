module Kit.Ast.UsingType where

import Kit.Ast.TypePath
import Kit.Str

data UsingType a b
  = UsingRuleSet TypePath
  deriving (Eq)

instance Show (UsingType a b) where
  show (UsingRuleSet tp) = "rules " ++ s_unpack (showTypePath tp)

convertUsingType :: UsingType a b -> UsingType c d
convertUsingType u =
  case u of
    UsingRuleSet t -> UsingRuleSet t
