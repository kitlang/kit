module Kit.Compiler.Unify where

import Control.Exception
import Control.Monad
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Error
import Kit.HashTable
import Kit.Parser.Span
import Kit.Str

-- Check whether type a unifies with b; i.e., can a value of type A be
-- assigned to a variable of type B?
unify
  :: CompileContext
  -> TypeContext
  -> Module
  -> ConcreteType
  -> ConcreteType
  -> IO TypeInformation
unify ctx tctx mod a b = do
  case (a, b) of
    (TypeTypeVar   v, x              ) -> return $ TypeVarIs v x
    (x              , TypeTypeVar v  ) -> unify ctx tctx mod b a
    (TypeBasicType a, TypeBasicType b) -> return $ unifyBasic a b
    (TypePtr       a, TypePtr b      ) -> unify ctx tctx mod a b
    _ -> return
      $ if a == b then TypeConstraintSatisfied else TypeConstraintNotSatisfied

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

resolveConstraint
  :: CompileContext -> TypeContext -> Module -> Span -> TypeConstraint -> IO ()
resolveConstraint ctx tctx mod pos constraint = do
  result <- resolveConstraintOrThrow ctx tctx mod pos constraint
  case result of
    TypeVarIs (TypeVar id) x -> do
      info <- getTypeVar ctx id
      h_insert (ctxTypeVariables ctx) id (info { typeVarValue = Just x })
      return ()
    TypeVarIs (TypeParamVar s) x -> do
      -- TODO
      return ()
    _ -> return ()

resolveConstraintOrThrow
  :: CompileContext
  -> TypeContext
  -> Module
  -> Span
  -> TypeConstraint
  -> IO TypeInformation
resolveConstraintOrThrow ctx tctx mod pos t = do
  result <- case t of
    TypeEq a b -> unify ctx tctx mod a b
  case result of
    TypeConstraintNotSatisfied -> constraintError t pos
    x                          -> return $ x

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
