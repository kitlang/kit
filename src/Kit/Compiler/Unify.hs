module Kit.Compiler.Unify where

import Control.Exception
import Control.Monad
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Ast.Span

data UnificationError = UnificationError CompileContext TypeConstraint
instance Errable UnificationError where
  logError err@(UnificationError ctx (TypeEq a b reason pos)) = do
    logErrorBasic err $ reason ++ ":"
    let showConstraint x = do case x of
                                TypeTypeVar i -> do
                                  info <- getTypeVar ctx i
                                  ePutStrLn $ show info
                                  let varPos = head $ typeVarPositions info
                                  displayFileSnippet varPos
                                _ -> ePutStrLn $ show x
    ePutStr $ "    Expected type:  "
    showConstraint a
    ePutStr $ "      Actual type:  "
    showConstraint b
    ePutStrLn ""
  errPos (UnificationError _ (TypeEq _ _ _ pos)) = Just pos
instance Show UnificationError where
  show (UnificationError _ tc) = "UnificationError " ++ show tc

instance Eq UnificationError where
  (==) (UnificationError _ c1) (UnificationError _ c2) = c1 == c2

getAbstractParents
  :: CompileContext -> TypeContext -> ConcreteType -> IO [ConcreteType]
getAbstractParents ctx tctx t = do
  case t of
    TypeInstance tp params -> do
      def <- getTypeDefinition ctx tp
      case typeSubtype def of
        Abstract { abstractUnderlyingType = t' } -> do
          let tctx' = addTypeParams
                tctx
                [ (typeSubPath def $ paramName param, value)
                | (param, value) <- zip (typeParams def) params
                ]
          t       <- mapType (follow ctx tctx') t'
          parents <- getAbstractParents ctx tctx' t
          return $ t : parents
        _ -> return []
    _ -> return []

unify ctx tctx a b = unifyBase ctx tctx False a b
unifyStrict ctx tctx a b = unifyBase ctx tctx True a b

{-
  Check whether type a unifies with b.
  LHS: variable type; RHS: value type
  i.e. trying to use RHS as a LHS
-}
unifyBase
  :: CompileContext
  -> TypeContext
  -> Bool
  -> ConcreteType
  -> ConcreteType
  -> IO (Maybe [TypeInformation])
unifyBase _   _    _      a  b | a == b = return $ Just []
unifyBase ctx tctx strict a' b'         = do
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
    (TypeSelf, x) -> case tctxSelf tctx of
      Just y  -> r y x
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
        (\((tp, params), _) -> r (TypeTraitConstraint (tp, params)) x)
      return $ checkResults ((Just $ [TypeVarIs i x]) : results)
    (_, TypeTypeVar _) -> r b a
    -- these two rules are a hack to support pointer arithmetic; would be
    -- better with a trait per operator and generic implementations for Ptr
    (TypeTraitConstraint ((["kit", "numeric"], "Numeric"), []), TypePtr _) ->
      do
        return $ Just []
    (TypeTraitConstraint ((["kit", "numeric"], "Integral"), []), TypePtr _) ->
      do
        return $ Just []
    (TypeTraitConstraint (tp1, params1), TypeBox tp2 params2) -> do
      if (tp1 == tp2) && (length params1 == length params2)
        then do
          paramMatch <- mapM (\(a, b) -> rStrict a b) (zip params1 params2)
          return $ checkResults paramMatch
        else return Nothing
    (TypeTraitConstraint t, x) -> do
      impl <- resolveTraitConstraint ctx tctx t x
      if impl then return $ Just [] else fallBackToAbstractParent a b
    (_, TypeTraitConstraint v) -> r b a
    (TypeBasicType a, TypeBasicType b) -> return $ unifyBasic a b
    (TypePtr (TypeBasicType BasicTypeVoid), TypePtr _) -> return $ Just []
    (TypePtr _, TypePtr (TypeBasicType BasicTypeVoid)) -> return $ Just []
    (TypePtr a, TypePtr b) -> r a b
    (TypeTuple a, TypeTuple b) | length a == length b -> do
      vals <- forM (zip a b) (\(a, b) -> r a b)
      return $ checkResults vals
    (TypeFunction rt1 args1 v1 _, TypeFunction rt2 args2 v2 _) | v1 == v2 -> do
      rt   <- r rt1 rt2
      args <- forM (zip args1 args2) (\((_, a), (_, b)) -> r a b)
      return $ checkResults $ rt : args
    (TypeEnumConstructor tp1 d1 _ params1, TypeEnumConstructor tp2 d2 _ params2)
      -> if (tp1 == tp2) && (d1 == d2) && (length params1 == length params2)
        then do
          paramMatch <- mapM (\(a, b) -> r a b) (zip params1 params2)
          return $ checkResults paramMatch
        else return Nothing
    (TypeBox tp1 params1, TypeBox tp2 params2) ->
      if (tp1 == tp2) && (length params1 == length params2)
        then do
          paramMatch <- mapM (\(a, b) -> rStrict a b) (zip params1 params2)
          return $ checkResults paramMatch
        else return Nothing
    (TypeArray t1 s1, TypeArray t2 s2) | (s1 > 0) && (s1 == s2) -> r t1 t2
    (TypeArray t1 0 , TypeArray t2 _ )                          -> r t1 t2
    (TypeArray t1 _ , TypePtr t2     )                          -> r t1 t2
    (TypeInstance tp1 params1, TypeInstance tp2 params2) ->
      if (tp1 == tp2) && (length params1 == length params2)
        then do
          paramMatch <- mapM (\(a, b) -> rStrict a b) (zip params1 params2)
          return $ checkResults paramMatch
        else if strict then return Nothing else fallBackToAbstractParent a b
    (_, TypeInstance tp1 params1) -> if a == b
      then return $ Just []
      else if strict then return Nothing else fallBackToAbstractParent a b
    (TypeInstance tp1 params1, _) ->
      -- in case of #[promote]
      fallBackToAbstractParent a b
    (TypeAnonStruct (Just a) _, TypeAnonStruct (Just b) _) ->
      if a == b then return $ Just [] else return Nothing
    (TypeAnonUnion (Just a) _, TypeAnonUnion (Just b) _) ->
      if a == b then return $ Just [] else return Nothing
    (TypeAnonEnum (Just a) _, TypeAnonEnum (Just b) _) ->
      if a == b then return $ Just [] else return Nothing
    _ -> return Nothing
 where
  r       = unifyBase ctx tctx strict
  rStrict = unifyStrict ctx tctx
  fallBackToAbstractParent a b = do
    parents <- getAbstractParents ctx tctx b
    if not (null parents)
      then r a (head $ reverse parents)
      else case a of
        TypeInstance tp params -> do
          t <- getTypeDefinition ctx tp
          case typeSubtype t of
            Abstract{} | hasMeta metaPromote (typeMeta t) -> do
              parents2 <- getAbstractParents ctx tctx a
              if not (null parents2)
                then r (head $ reverse parents2) b
                else return Nothing
            _ -> return Nothing
        _ -> return Nothing

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
  params <- forM params $ mapType $ follow ctx tctx
  impl   <- getTraitImpl ctx tctx (tp, params) ct
  return $ impl /= Nothing

{-
  Unify associated types when a trait implementation is selected.
-}
useImpl
  :: CompileContext
  -> TypeContext
  -> Span
  -> TraitDefinition a ConcreteType
  -> TraitImplementation a ConcreteType
  -> [ConcreteType]
  -> IO ()
useImpl ctx tctx pos traitDef impl params = do
  let assocParams = drop (length $ traitParams traitDef) params
  forM_ (zip (implAssocTypes impl) assocParams) $ \(p, val) -> resolveConstraint
    ctx
    tctx
    (TypeEq p val "Trait implementation has associated type" pos)


mergeVarInfo
  :: CompileContext
  -> TypeContext
  -> VarDefinition TypedExpr ConcreteType
  -> VarDefinition TypedExpr ConcreteType
  -> String
  -> IO ()
mergeVarInfo ctx tctx var1 var2 msg = resolveConstraint
  ctx
  tctx
  (TypeEq (varType var1) (varType var2) msg (varPos var1))

mergeArgInfo
  :: CompileContext
  -> TypeContext
  -> ArgSpec TypedExpr ConcreteType
  -> ArgSpec TypedExpr ConcreteType
  -> String
  -> IO ()
mergeArgInfo ctx tctx arg1 arg2 msg = do
  resolveConstraint ctx
                    tctx
                    (TypeEq (argType arg1) (argType arg2) msg (argPos arg1))

mergeFunctionInfo
  :: CompileContext
  -> TypeContext
  -> FunctionDefinition TypedExpr ConcreteType
  -> FunctionDefinition TypedExpr ConcreteType
  -> String
  -> String
  -> IO ()
mergeFunctionInfo ctx tctx f1 f2 rtMsg argMsg = do
  resolveConstraint
    ctx
    tctx
    (TypeEq (functionType f1) (functionType f2) rtMsg (functionPos f1))
  forM_ (zip (functionArgs f1) (functionArgs f2))
        (\(arg1, arg2) -> mergeArgInfo ctx tctx arg1 arg2 argMsg)
