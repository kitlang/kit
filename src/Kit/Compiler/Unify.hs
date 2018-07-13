module Kit.Compiler.Unify where

import Control.Exception
import Control.Monad
import Kit.Ast
import Kit.Compiler.Context
import Kit.Error
import Kit.HashTable
import Kit.Parser.Span
import Kit.Str

checkConstraint :: TypeConstraint -> TypeInformation
checkConstraint (TypeEq a b) = unify a b
checkConstraint (TypeClassMember cls (TypeTypeVar v)) =
  TypeVarHasConstraint v (TypeClassMember cls)
-- TODO
checkConstraint (TypeClassMember TypeNumeric (TypeBasicType (BasicTypeInt _)))
  = TypeConstraintSatisfied
checkConstraint (TypeClassMember TypeNumeric (TypeBasicType (BasicTypeUint _)))
  = TypeConstraintSatisfied
checkConstraint (TypeClassMember TypeNumeric (TypeBasicType (BasicTypeFloat _)))
  = TypeConstraintSatisfied
checkConstraint (TypeClassMember TypeIntegral (TypeBasicType (BasicTypeInt _)))
  = TypeConstraintSatisfied
checkConstraint (TypeClassMember TypeIntegral (TypeBasicType (BasicTypeUint _)))
  = TypeConstraintSatisfied
checkConstraint (TypeClassMember TypeNumericMixed (TypeBasicType (BasicTypeFloat _)))
  = TypeConstraintSatisfied
checkConstraint (TypeClassMember TypeString x) = TypeConstraintNotSatisfied
checkConstraint (TypeClassMember (TypeSequence t) x) =
  TypeConstraintNotSatisfied
checkConstraint (TypeClassMember (TypeIterable t) x) =
  TypeConstraintNotSatisfied
checkConstraint (TypeClassMember _ _) = TypeConstraintNotSatisfied

-- Check whether type a unifies with b; i.e., can a value of type A be
-- assigned to a variable of type B?
unify :: ConcreteType -> ConcreteType -> TypeInformation
unify x                 (TypeTypeVar v)   = TypeVarIs v x
unify (TypeTypeVar   v) x                 = TypeVarIs v x
unify (TypeBasicType a) (TypeBasicType b) = unifyBasic a b
unify (TypePtr       a) (TypePtr       b) = unify a b
unify a b =
  if a == b then TypeConstraintSatisfied else TypeConstraintNotSatisfied

unifyBasic :: BasicType -> BasicType -> TypeInformation
unifyBasic (BasicTypeVoid)    _                  = TypeConstraintNotSatisfied
unifyBasic _                  (BasicTypeVoid   ) = TypeConstraintNotSatisfied
unifyBasic (BasicTypeInt  _ ) (BasicTypeInt   _) = TypeConstraintSatisfied
unifyBasic (BasicTypeUint _ ) (BasicTypeInt   _) = TypeConstraintSatisfied
unifyBasic (BasicTypeInt  _ ) (BasicTypeUint  _) = TypeConstraintSatisfied
unifyBasic (BasicTypeUint _ ) (BasicTypeUint  _) = TypeConstraintSatisfied
unifyBasic (BasicTypeInt  _ ) (BasicTypeFloat _) = TypeConstraintSatisfied
unifyBasic (BasicTypeUint _ ) (BasicTypeFloat _) = TypeConstraintSatisfied
unifyBasic (BasicTypeUnknown) (_               ) = TypeConstraintNotSatisfied
unifyBasic a b =
  if a == b then TypeConstraintSatisfied else TypeConstraintNotSatisfied

resolveConstraint :: CompileContext -> Span -> TypeConstraint -> IO ()
resolveConstraint ctx pos constraint = do
  result <- resolveConstraintOrThrow pos constraint
  case result of
    TypeVarIs (TypeVar id) x -> do
      info <- getTypeVar ctx id
      h_insert (ctxTypeVariables ctx) id (info { typeVarValue = Just x })
      return ()
    TypeVarIs (TypeParamVar s) x -> do
      -- TODO
      return ()
    TypeVarHasConstraint (TypeVar id) x -> do
      info <- getTypeVar ctx id
      h_insert (ctxTypeVariables ctx) id (addTypeVarConstraint info x)
    _ -> return ()

resolveConstraintOrThrow :: Span -> TypeConstraint -> IO TypeInformation
resolveConstraintOrThrow pos t = do
  case checkConstraint t of
    TypeConstraintNotSatisfied -> constraintError t pos
    x                          -> return $ x

resolveConstraints :: CompileContext -> Span -> [TypeConstraint] -> IO ()
resolveConstraints ctx pos constraints =
  forM_ constraints (resolveConstraint ctx pos)

constraintError (TypeEq a b) pos = do
  throw $ Errs
    [ errp
        UnificationError
        ("Couldn't unify type constraints: " ++ (show a) ++ " and " ++ (show b))
        (Just pos)
    ]
constraintError t pos = do
  throw $ Errs
    [ errp UnificationError
           ("Couldn't satisfy type constraint: " ++ show t)
           (Just pos)
    ]
