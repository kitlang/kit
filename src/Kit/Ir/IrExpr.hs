{-# LANGUAGE DeriveGeneric #-}

module Kit.Ir.IrExpr where

import Data.Hashable
import GHC.Generics
import Kit.Str
import Kit.Ast
import Kit.Parser.Span

data IrExpr
  = IrBlock [IrExpr]
  | IrCompound [IrExpr]
  | IrLiteral (ValueLiteral BasicType)
  | IrIdentifier TypePath
  | IrPreUnop Operator IrExpr
  | IrPostUnop Operator IrExpr
  | IrBinop Operator IrExpr IrExpr
  | IrFor Str BasicType IrExpr IrExpr IrExpr
  | IrWhile IrExpr IrExpr Bool
  | IrIf IrExpr IrExpr (Maybe IrExpr)
  | IrContinue
  | IrBreak
  | IrReturn (Maybe IrExpr)
  | IrField IrExpr Str
  | IrArrayAccess IrExpr IrExpr
  | IrCall IrExpr [IrExpr]
  | IrCast IrExpr BasicType
  | IrCArrLiteral [IrExpr] BasicType
  | IrVarDeclaration Str BasicType (Maybe IrExpr)
  | IrStructInit BasicType [(Str, IrExpr)]
  | IrEnumInit BasicType TypePath [(Str, IrExpr)]
  | IrTupleInit BasicType [IrExpr]
  | IrSizeOf BasicType
  | IrSwitch IrExpr [(IrExpr, IrExpr)] (Maybe IrExpr)
  | IrNull
  | IrEmpty
  deriving (Eq, Show, Generic)

instance Hashable IrExpr

instance (Hashable t) => Hashable (ValueLiteral t) where
  hashWithSalt salt (BoolValue b) = (hashWithSalt salt b)
  hashWithSalt salt (IntValue i t) = (hashWithSalt salt i) + (hashWithSalt salt t)
  hashWithSalt salt (FloatValue s t) = (hashWithSalt salt s) + (hashWithSalt salt t)
  hashWithSalt salt (StringValue s) = (hashWithSalt salt s)
