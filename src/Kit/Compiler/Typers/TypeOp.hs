module Kit.Compiler.Typers.TypeOp where

import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Parser

{-
  Type check a unary operation.
-}
unopTypes
  :: Operator -> ConcreteType -> ConcreteType -> Span -> Maybe [TypeConstraint]
unopTypes op l x pos = case op of
  _ | (op == Inc) || (op == Dec) -> if isPtr l
    then Just
      [ TypeEq
          x
          l
          (  "Unary operator `"
          ++ show op
          ++ "` applied to a pointer returns an equivalent pointer"
          )
          pos
      ]
    else Just
      [ TypeEq
        typeClassNumeric
        l
        ("Unary operator `" ++ show op ++ "` can only be used on Numeric types")
        pos
      , TypeEq l x "An increment operation's type must match its operand" pos
      ]
  Dec -> Just
    [ TypeEq typeClassNumeric
             l
             "Decrement operator can only be used on Numeric types"
             pos
    , TypeEq l x "A decrement operation's type must match its operand" pos
    ]
  Invert -> Just
    [ TypeEq TypeBool
             l
             "Logical invert can only be used on Bool expressions"
             pos
    , TypeEq TypeBool
             x
             "A logical invert must yield a Bool"
             pos
    ]
  InvertBits -> Just
    [ TypeEq typeClassIntegral
             l
             "Bit invert can only be used on Integral types"
             pos
    , TypeEq l x "Bit invert must yield the same type as its operand" pos
    ]
  Ref -> Just
    [ TypeEq (TypePtr l)
             x
             "Reference operator must yield a pointer to its operand's type"
             pos
    ]
  Deref -> Just
    [ TypeEq
        (TypePtr x)
        l
        "Dereference operator must operate on a pointer, yielding the pointed to type"
        pos
    ]
  _ -> Nothing

{-
  Type check a binary operation.
-}
binopTypes
  :: Operator
  -> ConcreteType
  -> ConcreteType
  -> ConcreteType
  -> Bool
  -> Bool
  -> Span
  -> Maybe [TypeConstraint]
binopTypes op l r x lMixed rMixed pos = case op of
  Add        -> numericOrPointerOp
  Sub        -> numericOrPointerOp
  Mul        -> numericOp
  Div        -> numericOp
  Mod        -> numericOp
  Eq         -> comparisonOp
  Neq        -> comparisonOp
  Gte        -> comparisonOp
  Lte        -> comparisonOp
  LeftShift  -> numericOp
  RightShift -> numericOp
  Gt         -> comparisonOp
  Lt         -> comparisonOp
  And        -> booleanOp
  Or         -> booleanOp
  BitAnd     -> bitOp
  BitOr      -> bitOp
  BitXor     -> bitOp
  _          -> Nothing
 where
  numericOrPointerOp =
    let pointerOp a b = Just
          [ TypeEq
            x
            a
            (  "Binary operator `"
            ++ show op
            ++ "` applied to a pointer returns an equivalent pointer"
            )
            pos
          , TypeEq
            typeClassIntegral
            b
            (  "Binary operator `"
            ++ show op
            ++ "` requires an Integral value to apply to a pointer"
            )
            pos
          ]
    in  case (isPtr l, isPtr r) of
          (True , False) -> pointerOp l r
          (False, True ) -> pointerOp r l
          _              -> numericOp
  numericOp =
    Just
      $  (if (lMixed || rMixed)
           then
             [ TypeEq
                 typeClassNumericMixed
                 x
                 ("Binary operator `"
                 ++ show op
                 ++ "` requires a NumericMixed result if either operand is NumericMixed"
                 )
                 pos
             ]
           else []
         )
      ++ [ TypeEq
             typeClassNumeric
             i
             (  "Binary operator `"
             ++ show op
             ++ "` requires Numeric operands and result"
             )
             pos
         | i <- [l, r, x]
         ]
  comparisonOp = Just
    [ TypeEq
      l
      r
      (  "Comparison operator `"
      ++ show op
      ++ "` requires operands of similar type"
      )
      pos
    , TypeEq TypeBool x "Comparison operators must yield Bool values" pos
    ]
  booleanOp = Just
    [ TypeEq
        TypeBool
        i
        ("Binary operator `" ++ show op ++ "` requires Bool operands and result"
        )
        pos
    | i <- [l, r, x]
    ]
  bitOp = Just
    [ TypeEq
        typeClassIntegral
        i
        (  "Bitwise binary operator `"
        ++ show op
        ++ "` requires Integral operands and result"
        )
        pos
    | i <- [l, r, x]
    ]
