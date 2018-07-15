module Kit.Ast.Value where

import Kit.Str

data ValueLiteral
  = BooIdentifier Bool
  | IntValue Str
  | FloatValue Str
  | StringValue Str
  deriving (Eq, Show)
