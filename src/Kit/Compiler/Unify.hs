module Kit.Compiler.Unify where

import Control.Exception
import Control.Monad
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser.Span
import Kit.Str

data UnificationError = UnificationError CompileContext TypeConstraint deriving (Show)
instance Errable UnificationError where
  logError err@(UnificationError ctx (TypeEq a b reason pos)) = do
    logErrorBasic err $ reason ++ ":"
    ePutStrLn $ "Couldn't resolve the following constraints:\n"
    forM_ [a, b] (\x -> do case x of
                              TypeTypeVar i -> do
                                info <- getTypeVar ctx i
                                ePutStrLn $ "  - " ++ show info
                                let varPos = head $ typeVarPositions info
                                case file varPos of
                                  Just f -> displayFileSnippet (s_unpack f) varPos
                                  _ -> return ()
                              _ -> ePutStrLn $ "  - Type := " ++ show x
                           ePutStrLn "")
  errPos (UnificationError _ (TypeEq _ _ _ pos)) = Just pos

instance Eq UnificationError where
  (==) (UnificationError _ c1) (UnificationError _ c2) = c1 == c2

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
    (TypeTypeVar i, TypeTraitConstraint t) -> do
      info <- getTypeVar ctx i
      return $ if elem t (map fst $ typeVarConstraints info)
        then TypeConstraintSatisfied
        else TypeVarConstraint i t
    (TypeTypeVar i, x) -> do
      info <- getTypeVar ctx i
      let constraints = typeVarConstraints info
      meetsConstraints <- foldM
        (\acc ((tp, params), (reason, pos)) -> do
          case acc of
            TypeConstraintSatisfied ->
              unify ctx tctx mod (TypeTraitConstraint (tp, params)) x
            _ -> return acc
        )
        TypeConstraintSatisfied
        constraints
      return $ case meetsConstraints of
        TypeConstraintSatisfied -> TypeVarIs i x
        _                       -> meetsConstraints
    (_                    , TypeTypeVar _        ) -> unify ctx tctx mod b a
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
    TypeVarIs id x -> do
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
      info <- getTypeVar ctx id
      h_insert (ctxTypeVariables ctx) id (info { typeVarValue = Just x })
    TypeVarConstraint id constraint -> do
      info <- getTypeVar ctx id
      h_insert (ctxTypeVariables ctx)
               id
               (addTypeVarConstraints info constraint reason pos)
    _ -> return ()

resolveConstraintOrThrow
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypeConstraint
  -> IO TypeInformation
resolveConstraintOrThrow ctx tctx mod t@(TypeEq a' b' reason pos) = do
  a      <- knownType ctx tctx mod a'
  b      <- knownType ctx tctx mod b'
  result <- unify ctx tctx mod a b
  case result of
    TypeConstraintNotSatisfied -> throw $ KitError $ UnificationError ctx t
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
