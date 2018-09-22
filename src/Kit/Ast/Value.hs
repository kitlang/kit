module Kit.Ast.Value where

import Kit.Str

data ValueLiteral b
  = BoolValue Bool
  | IntValue Int b
  | FloatValue Str b
  | StringValue Str
  | CharValue Int
  deriving (Eq, Show)

valueEq :: ValueLiteral a -> ValueLiteral b -> Bool
valueEq (BoolValue a   ) (BoolValue b   ) = a == b
valueEq (IntValue   a _) (IntValue   b _) = a == b
valueEq (FloatValue a _) (FloatValue b _) = a == b
valueEq (StringValue a ) (StringValue b ) = a == b
valueEq (CharValue   a ) (CharValue   b ) = a == b
valueEq _                _                = False
