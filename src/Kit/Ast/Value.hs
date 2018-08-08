module Kit.Ast.Value where

import Kit.Str

data ValueLiteral
  = BoolValue Bool
  | IntValue Int
  | FloatValue Str
  | StringValue Str
  deriving (Eq, Show)
