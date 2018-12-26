module Kit.Compiler.Typers.AutoRefDeref where

import Control.Monad
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Unify
import Kit.Error

tryAutoRefDeref ctx tctx toType ex = do
  x <- _autoRefDeref ctx tctx toType (inferredType ex) ex
  case x of
    Just x  -> return x
    Nothing -> return ex

autoRefDeref ctx tctx toType ex =
  _autoRefDeref ctx tctx toType (inferredType ex) ex

{-
  Used to automatically perform specific implicit conversions:

  - referencing/dereferencing pointers
  - creating a Box pointer

  Attempts to convert expression `ex` from `fromType` to `toType`; returns
  either a new typed expression of type `fromType` or the original if the
  conversion wasn't possible.
-}
_autoRefDeref
  :: CompileContext
  -> TypeContext
  -> ConcreteType
  -> ConcreteType
  -> TypedExpr
  -> IO (Maybe TypedExpr)
_autoRefDeref ctx tctx (MethodTarget a) (MethodTarget b) ex = do
  result <- _autoRefDeref ctx tctx a b ex
  return $ case result of
    Just ex -> Just $ ex
      { inferredType = case inferredType ex of
        MethodTarget t -> MethodTarget t
        t              -> MethodTarget t
      }
    Nothing -> Nothing
_autoRefDeref ctx tctx toType fromType ex = do
  let r = _autoRefDeref ctx tctx
  toType   <- mapType (follow ctx tctx) toType
  fromType <- mapType (follow ctx tctx) fromType
  result   <- unifyStrict ctx tctx toType fromType
  case result of
    Just _ -> return $ Just ex
    _      -> case (toType, fromType) of
      (TypePtr f1@(TypeFunction _ _ _ _), f2@(TypeFunction _ _ _ _)) ->
        r f1 f2 ex
      (MethodTarget a   , _             ) -> return Nothing
      (_                , MethodTarget b) -> return Nothing
      (TypeBox tp params, b             ) -> do
        x <- makeBox ctx tctx tp params ex
        case x of
          Just x  -> return $ Just x
          Nothing -> return Nothing
      (TypePtr a, TypePtr b                 ) -> r a b ex
      (TypePtr a, b@(TypeInstance tp params)) -> do
        -- special case for abstracts over pointers
        result <- unifyStrict ctx tctx toType fromType
        case result of
          Just _  -> return $ Just ex
          Nothing -> r a b (addRef ex)
      (TypePtr a, b) -> do
        r a b (addRef ex)
      (a, TypePtr (TypeBasicType BasicTypeVoid)) ->
        -- don't try to deref a void pointer
        return Nothing
      (a, TypePtr b) -> case addDeref ex of
        Just x  -> r a b x
        Nothing -> return Nothing
      (TypeTuple a, TypeTuple b) | (length a == length b) && (isTupleInit ex) ->
        case tExpr ex of
          TupleInit t -> do
            parts <- forM (zip t (zip a b))
              $ \(val, (toType, fromType)) -> r toType fromType val
            if all isJust parts
              then do
                forMWithErrors_ (zip parts a) $ \(Just part, t) -> do
                  resolveConstraint
                    ctx
                    tctx
                    (TypeEq t
                            (inferredType part)
                            "Tuple parts must match declared type"
                            (tPos part)
                    )
                return $ Just $ makeExprTyped (TupleInit $ catMaybes parts)
                                              (TypeTuple a)
                                              (tPos ex)
              else return Nothing
          _ -> return Nothing
      (TypeInstance _ _, TypeInstance _ _) -> do
        result <- unify ctx tctx toType fromType
        case result of
          Just _ -> return $ Just ex
          _      -> return Nothing
      _ -> return Nothing

isTupleInit (TypedExpr { tExpr = TupleInit _ }) = True
isTupleInit _ = False

makeBox
  :: CompileContext
  -> TypeContext
  -> TypePath
  -> [ConcreteType]
  -> TypedExpr
  -> IO (Maybe TypedExpr)
makeBox ctx tctx tp params ex = do
  let (t, ref) = case inferredType ex of
        TypePtr t -> (t, ex)
        t         -> (t, addRef ex)
  traitDef <- getTraitDefinition ctx tp
  impl     <- getTraitImpl ctx tctx (tp, params) t
  case impl of
    Just impl -> do
      params <- makeGeneric ctx tp (tPos ex) params
      useImpl ctx tctx (tPos ex) traitDef impl (map snd params)
      t' <- mapType (follow ctx tctx) $ TypeBox tp $ map snd $ params
      return $ Just $ ex
        { tExpr        = Box
          (impl { implTrait = TypeTraitConstraint (tp, map snd params) })
          ref
        , inferredType = t'
        , tIsLocalPtr  = tIsLocal ref
        }
    Nothing -> return Nothing


addRef :: TypedExpr -> TypedExpr
addRef ex@(TypedExpr { tExpr = PreUnop Deref inner }) = inner
addRef ex@(TypedExpr { tIsLvalue = True }) =
  (makeExprTyped
      (PreUnop Ref ex)
      (case inferredType ex of
        MethodTarget t -> MethodTarget $ TypePtr t
        t              -> TypePtr t
      )
      (tPos ex)
    )
    { tIsLocalPtr = tIsLocal ex
    }
addRef ex = addRef (makeLvalue ex)

addDeref :: TypedExpr -> Maybe TypedExpr
addDeref (ex@(TypedExpr { tExpr = PreUnop Ref inner })) = Just inner
addDeref ex = case inferredType ex of
  TypePtr x -> Just $ makeExprTyped (PreUnop Deref ex) x (tPos ex)
  _         -> Nothing

makeLvalue :: TypedExpr -> TypedExpr
makeLvalue ex =
  ((makeExprTyped (Temp ex) (inferredType ex) (tPos ex)) { tIsLocal  = True
                                                         , tIsLvalue = True
                                                         }
  )
