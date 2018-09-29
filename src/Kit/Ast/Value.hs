{-# LANGUAGE DeriveGeneric #-}

module Kit.Ast.Value where

import Data.Hashable
import GHC.Generics
import Kit.Str

data ValueLiteral
  = BoolValue Bool
  | IntValue Int
  | FloatValue Str
  | StringValue Str
  deriving (Eq, Show, Generic)

instance Hashable ValueLiteral

valueEq :: ValueLiteral -> ValueLiteral -> Bool
valueEq (BoolValue   a) (BoolValue   b) = a == b
valueEq (IntValue    a) (IntValue    b) = a == b
valueEq (FloatValue  a) (FloatValue  b) = a == b
valueEq (StringValue a) (StringValue b) = a == b
valueEq _               _               = False
