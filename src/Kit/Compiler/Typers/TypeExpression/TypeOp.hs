module Kit.Compiler.Typers.TypeExpression.TypeOp (typeOp) where

import Control.Monad
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.AutoRefDeref
import Kit.Compiler.Typers.ExprTyper
import Kit.Compiler.Unify
import Kit.Error
import Kit.Str

typeOp :: SubTyper
typeOp (TyperUtils { _r = r, _tryRewrite = tryRewrite, _resolve = resolve, _typeExpr = typeExpr }) ctx tctx mod ex@(TypedExpr { tExpr = et, tPos = pos })
  = case et of
    (Binop Assign e1 e2) -> do
      r2 <- r e2
      tryRewrite (makeExprTyped (Binop Assign e1 r2) (inferredType ex) pos) $ do
        case tExpr e1 of
          TupleInit t -> do
            case inferredType r2 of
              TypeTuple t2 -> if length t == length t2
                then do
                  forMWithErrors_ (zip t t2) $ \(a, b) -> do
                    resolve $ TypeEq
                      (inferredType a)
                      b
                      "Tuple contents must match variables in tuple assignment"
                      pos

                  let
                    tupleExpr = if tIsLvalue r2
                      then r2
                      else (makeExprTyped (Temp r2) (inferredType r2) (tPos r2)) { tIsLocal = True
                                                                                 }

                  let boundSlots = filter
                        (\(i, (a, b)) -> tExpr a /= Identifier Hole)
                        (zip [0 ..] (zip t t2))
                  slots <- forMWithErrors boundSlots $ \(i, (a, b)) -> do
                    let
                      e1 = makeExprTyped
                        (Binop Assign
                               a
                               (makeExprTyped (TupleSlot tupleExpr i) b pos)
                        )
                        b
                        (tPos a)
                    case tExpr a of
                      Identifier _ -> return e1
                      _            -> r e1

                  return $ (makeExprTyped (Block slots) (inferredType r2) pos)
                else throwk $ TypingError
                  ("Tuples can only be assigned to tuples of the same size; actual type: "
                  ++ show (inferredType r2)
                  )
                  pos
              _ -> throwk $ TypingError
                ("Tuples can only be assigned to matching tuples; actual type: "
                ++ show (inferredType r2)
                )
                pos
          _ -> do
            r1        <- r e1
            converted <- tryAutoRefDeref ctx tctx (inferredType r1) r2
            when (tIsConst r1) $ throwk $ TypingError
              "Can't reassign a const value"
              pos
            resolve $ TypeEq
              (inferredType r1)
              (inferredType converted)
              "Assigned value must match the type of the lvalue it's assigned to"
              (tPos converted)
            return $ makeExprTyped (Binop Assign r1 converted)
                                   (inferredType r1)
                                   pos
    (Binop (AssignOp op) e1 e2) | (op == And) || (op == Or) -> do
      r1 <- r e1
      r2 <- r e2
      tryRewrite
          (makeExprTyped (Binop (AssignOp op) r1 r2) (inferredType ex) pos)
        $ do
            resolve $ TypeEq
              (inferredType r1)
              (inferredType r2)
              "Assigned value must match the type of the lvalue it's assigned to"
              (tPos r2)
            return $ makeExprTyped
              (Binop Assign
                     r1
                     (makeExprTyped (Binop op r1 r2) (inferredType r1) pos)
              )
              (inferredType r1)
              pos

    (Binop op@(AssignOp x) e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      tryRewrite (makeExprTyped (Binop op r1 r2) (inferredType ex) pos) $ do
        -- FIXME: this isn't right
        resolve $ TypeEq (inferredType r1)
                         (inferredType r2)
                         "FIXME: this isn't right"
                         pos
        return $ makeExprTyped (Binop op r1 r2) (inferredType r1) pos

    (Binop op@(Custom s) e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      tryRewrite (makeExprTyped (Binop op r1 r2) (inferredType ex) pos)
        $ throwk
        $ BasicError
            (  "Custom operator `"
            ++ s_unpack s
            ++ "` can only be used as part of a rewrite rule"
            )
            (Just pos)


    (Binop op e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      tryRewrite (makeExprTyped (Binop op r1 r2) (inferredType ex) pos) $ do
        lMixed <- unify ctx tctx (inferredType r1) (typeClassNumericMixed)
        rMixed <- unify ctx tctx (inferredType r2) (typeClassNumericMixed)
        let isMixed x = case x of
              Just x -> not $ or
                [ case i of
                    TypeVarConstraint _ _ -> True
                    _                     -> False
                | i <- x
                ]
              Nothing -> False
        let tv = inferredType ex
        case
            binopTypes op
                       (inferredType r1)
                       (inferredType r2)
                       tv
                       (isMixed lMixed)
                       (isMixed rMixed)
                       pos
          of
            Just constraints -> mapM_ resolve constraints
            Nothing          -> return () -- TODO
        let comp = case (tCompileTimeValue r1, tCompileTimeValue r2) of
              (Just x, Just y) -> interpOp op x y
              _                -> Nothing
        return $ (makeExprTyped (Binop op r1 r2) tv pos) { tCompileTimeValue = comp
                                                         }

    (PreUnop Ref e1@(TypedExpr { tExpr = This })) -> do
      -- referencing `this` gives us a pointer that's also an lvalue
      r1 <- r e1
      return $ (makeExprTyped (PreUnop Ref r1) (TypePtr $ inferredType r1) pos)
        { tIsLvalue   = True
        , tIsLocalPtr = tIsLocal r1
        }

    (PreUnop op e1) -> do
      r1 <- r e1
      tryRewrite (makeExprTyped (PreUnop op r1) (inferredType ex) pos) $ do
        case unopTypes op (inferredType r1) (inferredType ex) pos of
          Just constraints -> mapM_ resolve constraints
          Nothing          -> return () -- TODO
        return $ (makeExprTyped (PreUnop op r1) (inferredType ex) pos)
          { tIsLocalPtr = (op == Ref) && (tIsLocal r1)
          }

    (PostUnop op e1) -> do
      r1 <- r e1
      tryRewrite (makeExprTyped (PostUnop op r1) (inferredType ex) pos) $ do
        case unopTypes op (inferredType r1) (inferredType ex) pos of
          Just constraints -> mapM_ resolve constraints
          Nothing          -> return () -- TODO
        return $ makeExprTyped (PostUnop op r1) (inferredType ex) pos

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
    , TypeEq TypeBool x "A logical invert must yield a Bool" pos
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

interpOp :: Operator -> ValueLiteral -> ValueLiteral -> Maybe ValueLiteral
interpOp Add x y | valueIsNumber x && valueIsNumber y = Just $ x + y
interpOp Sub x y | valueIsNumber x && valueIsNumber y = Just $ x - y
interpOp Mul x y | valueIsNumber x && valueIsNumber y = Just $ x * y
interpOp Div x y | valueIsNumber x && valueIsNumber y = Just $ x / y
interpOp Eq  x y = Just $ BoolValue $ valueEq x y
interpOp Neq x y = Just $ BoolValue $ not $ valueEq x y
interpOp _   _ _ = Nothing
