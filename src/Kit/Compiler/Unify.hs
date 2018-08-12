module Kit.Compiler.Unify where

import Control.Exception
import Control.Monad
import Data.List
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
                                displayFileSnippet varPos
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
  -> IO (Maybe [TypeInformation])
unify ctx tctx mod a' b' = do
  let checkResults x = foldr
        (\result acc -> case acc of
          Just x -> case result of
            Just y  -> Just (x ++ y)
            Nothing -> Nothing
          Nothing -> Nothing
        )
        (Just [])
        x
  a <- knownType ctx tctx mod a'
  b <- knownType ctx tctx mod b'
  case (a, b) of
    (TypeTypeVar i, TypeTraitConstraint t) -> do
      info <- getTypeVar ctx i
      return $ if elem t (map fst $ typeVarConstraints info)
        then Just []
        else Just [TypeVarConstraint i t]
    (TypeTypeVar a, TypeTypeVar b) -> do
      info1 <- getTypeVar ctx a
      info2 <- getTypeVar ctx b
      return $ if (typeVarId info1 == typeVarId info2)
        then Just []
        else Just [TypeVarIs a (TypeTypeVar b)]
    (TypeTypeVar i, x) -> do
      info <- getTypeVar ctx i
      let constraints = typeVarConstraints info
      results <- forM
        constraints
        (\((tp, params), _) ->
          unify ctx tctx mod (TypeTraitConstraint (tp, params)) x
        )
      return $ checkResults ((Just $ [TypeVarIs i x]) : results)
    (_                    , TypeTypeVar _) -> unify ctx tctx mod b a
    (TypeTraitConstraint t, x            ) -> do
      impl <- resolveTraitConstraint ctx tctx mod t x
      return $ if impl then Just [] else Nothing
    (_, TypeTraitConstraint v) -> unify ctx tctx mod b a
    (TypeBasicType a, TypeBasicType b) -> return $ unifyBasic a b
    (TypePtr (TypeBasicType BasicTypeVoid), TypePtr _) -> return $ Just []
    (TypePtr _, (TypeBasicType BasicTypeVoid)) -> return $ Just []
    (TypePtr a, TypePtr b) -> unify ctx tctx mod a b
    (TypeTuple a, TypeTuple b) | length a == length b -> do
      vals <- forM (zip a b) (\(a, b) -> unify ctx tctx mod a b)
      return $ checkResults vals
    (TypeFunction rt1 args1 v1, TypeFunction rt2 args2 v2) | v1 == v2 -> do
      rt   <- unify ctx tctx mod rt1 rt2
      args <- forM (zip args1 args2)
                   (\((_, a), (_, b)) -> unify ctx tctx mod a b)
      return $ checkResults $ rt : args
    _ -> return $ if a == b then Just [] else Nothing

unifyBasic :: BasicType -> BasicType -> Maybe [TypeInformation]
unifyBasic (BasicTypeVoid)    (BasicTypeVoid)    = Just []
unifyBasic (BasicTypeVoid)    _                  = Nothing
unifyBasic _                  (BasicTypeVoid   ) = Nothing
unifyBasic (BasicTypeInt  _ ) (BasicTypeInt   _) = Just []
unifyBasic (BasicTypeUint _ ) (BasicTypeInt   _) = Just []
unifyBasic (BasicTypeInt  _ ) (BasicTypeUint  _) = Just []
unifyBasic (BasicTypeUint _ ) (BasicTypeUint  _) = Just []
unifyBasic (BasicTypeInt  _ ) (BasicTypeFloat _) = Just []
unifyBasic (BasicTypeUint _ ) (BasicTypeFloat _) = Just []
unifyBasic (BasicTypeUnknown) (_               ) = Nothing
unifyBasic (CPtr a          ) (CPtr b          ) = unifyBasic a b
unifyBasic a b = if a == b then Just [] else Nothing

resolveConstraint
  :: CompileContext -> TypeContext -> Module -> TypeConstraint -> IO ()
resolveConstraint ctx tctx mod constraint@(TypeEq a b reason pos) = do
  results <- resolveConstraintOrThrow ctx tctx mod constraint
  forM_ results $ \result -> case result of
    TypeVarIs a (TypeTypeVar b) | a == b -> do
      -- tautological; would cause an endless loop
      return ()
    TypeVarIs a x@(TypeTypeVar b) -> do
      info1 <- getTypeVar ctx a
      info2 <- getTypeVar ctx b
      when ((typeVarId info1) /= (typeVarId info2)) $ do
        let constraints =
              nub $ (typeVarConstraints info1) ++ (typeVarConstraints info2)
        h_insert (ctxTypeVariables ctx)
                 (typeVarId info1)
                 (info1 { typeVarValue = Just x })
        h_insert (ctxTypeVariables ctx)
                 (typeVarId info2)
                 (info2 { typeVarConstraints = constraints })
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
      h_insert (ctxTypeVariables ctx)
               (typeVarId info)
               (info { typeVarValue = Just x })
    TypeVarConstraint id constraint -> do
      info <- getTypeVar ctx id
      h_insert (ctxTypeVariables ctx)
               (typeVarId info)
               (addTypeVarConstraints info constraint reason pos)

resolveConstraintOrThrow
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypeConstraint
  -> IO [TypeInformation]
resolveConstraintOrThrow ctx tctx mod t@(TypeEq a' b' reason pos) = do
  a      <- knownType ctx tctx mod a'
  b      <- knownType ctx tctx mod b'
  result <- unify ctx tctx mod a b
  case result of
    Just x  -> return $ x
    Nothing -> throw $ KitError $ UnificationError ctx t

resolveTraitConstraint
  :: CompileContext
  -> TypeContext
  -> Module
  -> TraitConstraint
  -> ConcreteType
  -> IO Bool
resolveTraitConstraint ctx tctx mod (tp, params) ct = do
  impl <- getTraitImpl ctx tp ct
  return $ case impl of
    Just _ -> True
    _      -> False
