module Kit.Compiler.Typers.TypeLiteral where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TermRewrite
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.AutoRefDeref
import Kit.Compiler.Typers.Base
import Kit.Compiler.Typers.ConvertExpr
import Kit.Compiler.Unify
import Kit.Error
import Kit.HashTable
import Kit.Parser
import Kit.Str

{-
  Returns a set of type constraints for literals depending on their value.
-}
literalConstraints :: ValueLiteral b -> ConcreteType -> Span -> [TypeConstraint]
literalConstraints (BoolValue _) s pos =
  [TypeEq (basicType $ BasicTypeBool) s "Bool literal must be a Bool type" pos]
literalConstraints (IntValue v _) s pos =
  let exps = if v < 0
        then [-63, -53, -31, -24, -15, -7, 0] :: [Int]
        else [0, 7, 8, 15, 16, 24, 31, 32, 53, 63, 64] :: [Int]
  in
    let
      t = foldr
        (\(low, high) acc ->
          if v
               >= (signum low)
               *  (2 ^ abs low)
               && v
               <  (signum high)
               *  (2 ^ abs high)
            then typeClassRange (if v < 0 then low else high)
            else acc
        )
        typeClassNumeric
        (zip exps (drop 1 exps))
    in  [TypeEq t s "Int literals must be a Numeric type" pos]
literalConstraints (FloatValue _ _) s pos =
  [ TypeEq typeClassNumericMixed
           s
           "Float literals must be a NumericMixed type"
           pos
  ]
literalConstraints (StringValue _) s pos =
  [TypeEq typeClassStringy s "String literals must be a Stringy type" pos]
