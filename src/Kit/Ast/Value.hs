module Kit.Ast.Value where

import Kit.Str

data ValueLiteral
  = BoolValue Bool
  | IntValue Str
  | FloatValue Str
  | StringValue Str
  deriving (Eq, Show)
