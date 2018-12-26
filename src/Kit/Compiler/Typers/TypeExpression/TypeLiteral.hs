module Kit.Compiler.Typers.TypeExpression.TypeLiteral (literalConstraints) where

import Data.List
import Kit.Ast
import Kit.Parser

{-
  Returns a set of type constraints for literals depending on their value.
-}
literalConstraints :: ValueLiteral -> ConcreteType -> Span -> [TypeConstraint]
literalConstraints (BoolValue _) s pos =
  [TypeEq TypeBool s "Bool literal must be a Bool type" pos]
literalConstraints (IntValue v) s pos =
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
literalConstraints (FloatValue _) s pos =
  [ TypeEq typeClassNumericMixed
           s
           "Float literals must be a NumericMixed type"
           pos
  ]
literalConstraints _ _ _ = []
