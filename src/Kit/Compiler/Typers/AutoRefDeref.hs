module Kit.Compiler.Typers.AutoRefDeref where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
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
  let
    tryLvalue a b = do
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
          _autoRefDeref ctx tctx a b (temp : temps)
            $ (makeExprTyped (Identifier (Var ([], tmp)))
                             (inferredType ex)
                             (tPos ex)
              ) { tIsLvalue = True
                }
  let finalizeResult ex = do
        case tctxTemps tctx of
          Just v -> do
            modifyIORef v (\val -> val ++ reverse temps)
        return $ Just ex
  toType   <- follow ctx tctx toType
  fromType <- follow ctx tctx fromType
  result   <- unify ctx tctx toType fromType
  case result of
    Just _ -> finalizeResult ex
    _      -> case (toType, fromType) of
      (TypeBox tp params, b) -> do
        if tIsLvalue ex
          then do
            box <- makeBox ctx tctx tp params ex
            case box of
              Just x  -> finalizeResult x
              Nothing -> return Nothing
          else tryLvalue toType fromType
      (TypePtr a, TypePtr b) -> _autoRefDeref ctx tctx a b temps ex
      (TypePtr a, b        ) -> if tIsLvalue ex
        then _autoRefDeref ctx tctx a b temps (addRef ex)
        else tryLvalue toType fromType
      (a, TypePtr (TypeBasicType BasicTypeVoid)) ->
        -- don't try to deref a void pointer
        return Nothing
      (a, TypePtr b) -> _autoRefDeref ctx tctx a b temps (addDeref ex)
      -- TODO: autorefderef tuples
      _              -> return Nothing

makeBox
  :: CompileContext
  -> TypeContext
  -> TypePath
  -> [ConcreteType]
  -> TypedExpr
  -> IO (Maybe TypedExpr)
makeBox ctx tctx tp params ex = do
  if tIsLvalue ex
    then do
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
              ex
            , inferredType = t'
            }
        Nothing -> return Nothing
    else return Nothing

addRef :: TypedExpr -> TypedExpr
addRef ex =
  makeExprTyped (PreUnop Ref ex) (TypePtr $ inferredType ex) (tPos ex)

addDeref :: TypedExpr -> TypedExpr
addDeref ex = case inferredType ex of
  TypePtr x -> makeExprTyped (PreUnop Deref ex) x (tPos ex)
  _         -> undefined
