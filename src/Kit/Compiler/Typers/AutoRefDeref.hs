module Kit.Compiler.Typers.AutoRefDeref where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TermRewrite
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.Base
import Kit.Compiler.Typers.ConvertExpr
import Kit.Compiler.Unify
import Kit.Error
import Kit.HashTable
import Kit.Parser
import Kit.Str

tryAutoRefDeref ctx tctx toType ex = do
  x <- _autoRefDeref ctx tctx toType (inferredType ex) [] ex
  case x of
    Just x  -> return x
    Nothing -> return ex

autoRefDeref ctx tctx toType ex =
  _autoRefDeref ctx tctx toType (inferredType ex) [] ex

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
  -> [TypedExpr]
  -> TypedExpr
  -> IO (Maybe TypedExpr)
_autoRefDeref ctx tctx toType fromType temps ex = do
  let finalizeResult ex = do
        addTemps tctx temps
        return $ Just ex
  toType   <- mapType (follow ctx tctx) toType
  fromType <- mapType (follow ctx tctx) fromType
  result   <- unify ctx tctx toType fromType
  case result of
    Just _ -> finalizeResult ex
    _      -> case (toType, fromType) of
      (TypeBox tp params, b) -> do
        x <- makeBox ctx tctx tp params ex
        case x of
          Just x  -> finalizeResult x
          Nothing -> return Nothing
      (TypePtr a, TypePtr b) -> _autoRefDeref ctx tctx a b temps ex
      (TypePtr a, b        ) -> case addRef ex of
        Just x  -> _autoRefDeref ctx tctx a b temps x
        Nothing -> return Nothing
      (a, TypePtr (TypeBasicType BasicTypeVoid)) ->
        -- don't try to deref a void pointer
        return Nothing
      (a, TypePtr b) -> case addDeref ex of
        Just x  -> _autoRefDeref ctx tctx a b temps x
        Nothing -> return Nothing
      (TypeTuple a, TypeTuple b) | (length a == length b) && (isTupleInit ex) ->
        case tExpr ex of
          TupleInit t -> do
            parts <- forM (zip t (zip a b)) $ \(val, (toType, fromType)) ->
              _autoRefDeref ctx tctx toType fromType temps val
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
                finalizeResult $ makeExprTyped (TupleInit $ catMaybes parts)
                                               (TypeTuple a)
                                               (tPos ex)
              else return Nothing
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
  case addRef ex of
    Just ref -> do
      traitDef <- getTraitDefinition ctx tp
      impl     <- getTraitImpl ctx tctx (tp, params) (inferredType ex)
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
            }
        Nothing -> return Nothing
    Nothing -> do
      lvalue <- makeLvalue tctx ex
      case lvalue of
        Just lvalue -> makeBox ctx tctx tp params lvalue

addRef :: TypedExpr -> Maybe TypedExpr
addRef ex@(TypedExpr { tExpr = PreUnop Deref inner }) = Just inner
addRef ex@(TypedExpr { tIsLvalue = True }) =
  Just $ makeExprTyped (PreUnop Ref ex) (TypePtr $ inferredType ex) (tPos ex)
addRef ex = Nothing

addDeref :: TypedExpr -> Maybe TypedExpr
addDeref (ex@(TypedExpr { tExpr = PreUnop Ref inner })) = Just inner
addDeref ex = case inferredType ex of
  TypePtr x -> Just $ makeExprTyped (PreUnop Deref ex) x (tPos ex)
  _         -> Nothing

makeLvalue :: TypeContext -> TypedExpr -> IO (Maybe TypedExpr)
makeLvalue tctx ex = do
  case tctxTemps tctx of
    Just v -> do
      tmp <- makeTmpVar (head $ tctxScopes tctx)
      let temp =
            (makeExprTyped
              (VarDeclaration (Var ([], tmp))
                              (inferredType ex)
                              (Just $ ex { tTemps = [] })
              )
              (inferredType ex)
              (tPos ex)
            )
      addTemps tctx [temp]
      return $ Just (makeExprTyped (Identifier (Var ([], tmp)))
                                   (inferredType ex)
                                   (tPos ex)
                    )
        { tIsLvalue = True
        }
    Nothing -> return Nothing
