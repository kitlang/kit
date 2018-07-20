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

data UnificationError = UnificationError TypeConstraint deriving (Eq, Show)
instance Errable UnificationError where
  logError e@(UnificationError (TypeEq a b reason pos)) =
    logErrorBasic e $ "Couldn't unify type constraints:\n\n  - `" ++ show a ++ "`\n  - `" ++ show b ++ "`\n\n" ++ reason
  errPos (UnificationError (TypeEq _ _ _ pos)) = Just pos

-- Check whether type a unifies with b; i.e., can a value of type A be
-- assigned to a variable of type B?
unify
  :: CompileContext
  -> TypeContext
  -> Module
  -> ConcreteType
  -> ConcreteType
  -> IO TypeInformation
unify ctx tctx mod a' b' = do
  a <- knownType ctx tctx mod a'
  b <- knownType ctx tctx mod b'
  case (a, b) of
    (TypeTypeVar v@(TypeVar i), TypeTraitConstraint t) -> do
      info <- getTypeVar ctx i
      return $ if elem t (map fst $ typeVarConstraints info)
        then TypeConstraintSatisfied
        else TypeVarConstraint v t
    (TypeTypeVar v        , x                    ) -> return $ TypeVarIs v x
    (_                    , TypeTypeVar v        ) -> unify ctx tctx mod b a
    (TypeTraitConstraint t, x                    ) -> resolveTraitConstraint ctx tctx mod t x
    (_                    , TypeTraitConstraint v) -> unify ctx tctx mod b a
    (TypeBasicType a      , TypeBasicType b      ) -> return $ unifyBasic a b
    (TypePtr       a      , TypePtr b            ) -> unify ctx tctx mod a b
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
unifyBasic (CPtr a          ) (CPtr b          ) = unifyBasic a b
unifyBasic a b =
  if a == b then TypeConstraintSatisfied else TypeConstraintNotSatisfied

resolveConstraint
  :: CompileContext -> TypeContext -> Module -> TypeConstraint -> IO ()
resolveConstraint ctx tctx mod constraint@(TypeEq a b reason pos) = do
  result <- resolveConstraintOrThrow ctx tctx mod constraint
  case result of
    TypeVarIs (TypeVar id) x -> do
      info <- getTypeVar ctx id
      let constraints = typeVarConstraints info
      forM_
        (constraints)
        (\(constraint, (reason', pos')) -> resolveConstraint
          ctx
          tctx
          mod
          (TypeEq x (TypeTraitConstraint constraint) reason' pos')
        )
      h_insert (ctxTypeVariables ctx) id (info { typeVarValue = Just x })
      return ()
    TypeVarConstraint (TypeVar id) constraint -> do
      info <- getTypeVar ctx id
      h_insert (ctxTypeVariables ctx)
               id
               (addTypeVarConstraints info constraint reason pos)
    TypeVarIs (TypeParamVar s) x -> do
      -- TODO
      return ()
    _ -> return ()

resolveConstraintOrThrow
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypeConstraint
  -> IO TypeInformation
resolveConstraintOrThrow ctx tctx mod t@(TypeEq a b reason pos) = do
  result <- unify ctx tctx mod a b
  case result of
    TypeConstraintNotSatisfied -> throw $ KitError $ UnificationError t
    x                          -> return $ x

resolveTraitConstraint
  :: CompileContext
  -> TypeContext
  -> Module
  -> TraitConstraint
  -> ConcreteType
  -> IO TypeInformation
resolveTraitConstraint ctx tctx mod (tp, params) ct = do
  impl <- getTraitImpl ctx tp ct
  return $ case impl of
    Just _ -> TypeConstraintSatisfied
    _      -> TypeConstraintNotSatisfied
