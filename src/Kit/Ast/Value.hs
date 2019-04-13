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
  deriving (Eq, Generic)

instance Hashable ValueLiteral

instance Show ValueLiteral where
  show (BoolValue b) = if b then "true" else "false"
  show (IntValue i) = show i
  show (FloatValue s) = s_unpack s
  show (StringValue s) = show $ s_unpack s

valueEq :: ValueLiteral -> ValueLiteral -> Bool
valueEq (BoolValue   a) (BoolValue   b) = a == b
valueEq (IntValue    a) (IntValue    b) = a == b
valueEq (FloatValue  a) (FloatValue  b) = a == b
-- valueEq (IntValue    a) (FloatValue  b) = a == b
-- valueEq (FloatValue  a) (IntValue    b) = a == b
valueEq (StringValue a) (StringValue b) = a == b
valueEq _               _               = False

valueIsNumber (IntValue a) = True
-- valueIsNumber (FloatValue a) = True
valueIsNumber _            = False

valueNumber :: ValueLiteral -> Int
valueNumber (IntValue a) = a

instance Num ValueLiteral where
  (IntValue x) + (IntValue y) = IntValue (x + y)
  (IntValue x) - (IntValue y) = IntValue (x - y)
  (IntValue x) * (IntValue y) = IntValue (x * y)
  fromInteger x = IntValue $ fromInteger x
  abs (IntValue x) = IntValue $ abs x
  signum (IntValue x) = IntValue $ abs x

instance Fractional ValueLiteral where
  (IntValue x) / (IntValue y) = IntValue (x `quot` y)
  -- fromRational (IntValue x) = IntValue $ fromRational $ fromInteger x
  -- fromRational (FloatValue x) = FloatValue $ fromRational x
