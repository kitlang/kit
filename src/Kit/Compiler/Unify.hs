module Kit.Compiler.Unify where

import Control.Exception
import Control.Monad
import Data.List
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser.Span
import Kit.Str

data UnificationError = UnificationError CompileContext TypeConstraint
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
instance Show UnificationError where
  show (UnificationError _ tc) = "UnificationError " ++ show tc

instance Eq UnificationError where
  (==) (UnificationError _ c1) (UnificationError _ c2) = c1 == c2

getAbstractParents
  :: CompileContext -> TypeContext -> ConcreteType -> IO [ConcreteType]
getAbstractParents ctx tctx t = do
  case t of
    TypeInstance (modPath, typeName) params -> do
      def <- getTypeDefinition ctx modPath typeName
      case typeSubtype def of
        Abstract { abstractUnderlyingType = t' } -> do
          let tctx' = addTypeParams
                tctx
                [ (paramName param, value)
                | (param, value) <- zip (typeParams def) params
                ]
          t       <- mapType (follow ctx tctx') t'
          parents <- getAbstractParents ctx tctx' t
          return $ t : parents
        _ -> return []
    _ -> return []

{-
  Check whether type a unifies with b.
  LHS: variable type; RHS: value type
  i.e. trying to use RHS as a LHS
-}
unify
  :: CompileContext
  -> TypeContext
  -> ConcreteType
  -> ConcreteType
  -> IO (Maybe [TypeInformation])
unify ctx tctx a' b' = do
  let checkResults x = foldr
        (\result acc -> case acc of
          Just x -> case result of
            Just y  -> Just (x ++ y)
            Nothing -> Nothing
          Nothing -> Nothing
        )
        (Just [])
        x
  a <- mapType (follow ctx tctx) a'
  b <- mapType (follow ctx tctx) b'
  case (a, b) of
    (TypeSelf, TypeSelf) -> return $ Just []
    (TypeSelf, x) -> case tctxSelf tctx of
      Just y  -> unify ctx tctx y x
      Nothing -> return Nothing
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
          unify ctx tctx (TypeTraitConstraint (tp, params)) x
        )
      return $ checkResults ((Just $ [TypeVarIs i x]) : results)
    (_                    , TypeTypeVar _) -> unify ctx tctx b a
    (TypeTraitConstraint t, x            ) -> do
      impl <- resolveTraitConstraint ctx tctx t x
      if impl then return $ Just [] else fallBackToAbstractParent a b
    (_, TypeTraitConstraint v) -> unify ctx tctx b a
    (TypeBasicType a, TypeBasicType b) -> return $ unifyBasic a b
    (TypePtr (TypeBasicType BasicTypeVoid), TypePtr _) -> return $ Just []
    (TypePtr _, (TypeBasicType BasicTypeVoid)) -> return $ Just []
    (TypePtr a, TypePtr b) -> unify ctx tctx a b
    (TypeTuple a, TypeTuple b) | length a == length b -> do
      vals <- forM (zip a b) (\(a, b) -> unify ctx tctx a b)
      return $ checkResults vals
    (TypeFunction rt1 args1 v1 _, TypeFunction rt2 args2 v2 _) | v1 == v2 -> do
      rt   <- unify ctx tctx rt1 rt2
      args <- forM (zip args1 args2) (\((_, a), (_, b)) -> unify ctx tctx a b)
      return $ checkResults $ rt : args
    (TypeInstance tp1 params1, TypeInstance tp2 params2) -> do
      if (tp1 == tp2) && (length params1 == length params2)
        then do
          paramMatch <- mapM (\(a, b) -> unify ctx tctx a b)
                             (zip params1 params2)
          return $ checkResults paramMatch
        else fallBackToAbstractParent a b
    (_, TypeInstance tp1 params1) -> do
      if a == b then return $ Just [] else fallBackToAbstractParent a b
    (TypeEnumConstructor tp1 d1 _ params1, TypeEnumConstructor tp2 d2 _ params2)
      -> do
        if (tp1 == tp2) && (d1 == d2) && (length params1 == length params2)
          then do
            paramMatch <- mapM (\(a, b) -> unify ctx tctx a b)
                               (zip params1 params2)
            return $ checkResults paramMatch
          else return Nothing
    (a, b) | a == b -> return $ Just []
    _               -> return Nothing
 where
  fallBackToAbstractParent a b = do
    parents <- getAbstractParents ctx tctx b
    if null parents
      then return Nothing
      else unify ctx tctx a (head $ reverse parents)

unifyBasic :: BasicType -> BasicType -> Maybe [TypeInformation]
unifyBasic (BasicTypeVoid)    (BasicTypeVoid) = Just []
unifyBasic (BasicTypeVoid)    _               = Nothing
unifyBasic _                  (BasicTypeVoid) = Nothing
unifyBasic a                  b | (typeIsIntegral a) && (typeIsIntegral b) = Just []
unifyBasic (BasicTypeFloat _) b | (typeIsIntegral b) = Just []
unifyBasic (BasicTypeUnknown) (_     )        = Nothing
unifyBasic (CPtr a          ) (CPtr b)        = unifyBasic a b
unifyBasic a b = if a == b then Just [] else Nothing

resolveConstraint :: CompileContext -> TypeContext -> TypeConstraint -> IO ()
resolveConstraint ctx tctx constraint@(TypeEq a b reason pos) = do
  results <- resolveConstraintOrThrow ctx tctx constraint
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
        unresolved <- h_exists (ctxUnresolvedTypeVars ctx) (typeVarId info1)
        when unresolved $ h_delete (ctxUnresolvedTypeVars ctx) (typeVarId info1)
    TypeVarIs id x -> do
      info <- getTypeVar ctx id
      let constraints = typeVarConstraints info
      forM_
        (constraints)
        (\(constraint, (reason', pos')) -> resolveConstraint
          ctx
          tctx
          (TypeEq x (TypeTraitConstraint constraint) reason' pos')
        )
      info <- getTypeVar ctx id
      h_insert (ctxTypeVariables ctx)
               (typeVarId info)
               (info { typeVarValue = Just x })
      unresolved <- h_exists (ctxUnresolvedTypeVars ctx) (typeVarId info)
      when unresolved $ h_delete (ctxUnresolvedTypeVars ctx) (typeVarId info)
    TypeVarConstraint id constraint -> do
      info <- getTypeVar ctx id
      h_insert (ctxTypeVariables ctx)
               (typeVarId info)
               (addTypeVarConstraints info constraint reason pos)

resolveConstraintOrThrow
  :: CompileContext -> TypeContext -> TypeConstraint -> IO [TypeInformation]
resolveConstraintOrThrow ctx tctx t@(TypeEq a' b' reason pos) = do
  a      <- follow ctx tctx a'
  b      <- follow ctx tctx b'
  result <- unify ctx tctx a b
  case result of
    Just x  -> return $ x
    Nothing -> throw $ KitError $ UnificationError ctx t

resolveTraitConstraint
  :: CompileContext -> TypeContext -> TraitConstraint -> ConcreteType -> IO Bool
resolveTraitConstraint ctx tctx (tp, params) ct = do
  impl <- getTraitImpl ctx tctx tp ct
  return $ impl /= Nothing
