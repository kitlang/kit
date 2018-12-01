module Kit.Compiler.Typers.TypeExpression.TypeCast where

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.AutoRefDeref
import Kit.Compiler.Typers.ExprTyper
import Kit.Compiler.Typers.TypeExpression.ExprToType
import Kit.Compiler.Typers.TypeExpression.TypeVarBinding
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.Str

typeCast :: SubTyper
typeCast (TyperUtils { _r = r, _tryRewrite = tryRewrite, _resolve = resolve, _typeExpr = typeExpr }) ctx tctx mod ex@(TypedExpr { tExpr = et, tPos = pos })
  = case et of
    (Cast e1 t) -> do
      t  <- mapType (follow ctx tctx) t
      t  <- makeGenericConcrete ctx pos t
      r1 <- r e1
      tryRewrite (makeExprTyped (Cast r1 t) t pos) $ do
        let cast = return $ makeExprTyped (Cast r1 t) t pos
        let invalidCast = throwk $ TypingError
              ("Invalid cast: " ++ show (inferredType r1) ++ " as " ++ show t)
              pos
        let
          typeCast rt = case (rt, t) of
            (TypeBox _ _, TypePtr (TypeBasicType BasicTypeVoid)) ->
              return $ makeExprTyped (BoxedValue r1) (TypePtr TypeVoid) pos
            (TypePtr _    , TypePtr (TypeBasicType BasicTypeVoid)) -> cast
            (TypeArray _ _, TypePtr (TypeBasicType BasicTypeVoid)) -> cast
            (TypeArray t _, TypePtr t2                           ) -> do
              t' <- unifyStrict ctx tctx t2 t
              case t' of
                Just _ -> cast
                _      -> invalidCast
            (TypePtr _, TypeBasicType BasicTypeCSize) -> cast
            (x        , y@(TypeBox tp params)       ) -> do
              box <- autoRefDeref ctx tctx y r1
              case box of
                Just box -> return box
                _        -> throwk $ TypingError
                  (  show x
                  ++ " can't be converted to a "
                  ++ show y
                  ++ "; no matching trait implementation found"
                  )
                  pos
            (x@(TypeInstance tp params), y) -> do
              t' <- unify ctx tctx x y
              case t' of
                Just x -> cast
                _      -> do
                  templateDef <- getTypeDefinition ctx tp
                  let
                    tctx' =
                      (addTypeParams
                          tctx
                          [ (typeSubPath templateDef $ paramName param, val)
                          | (param, val) <- zip (typeParams templateDef) params
                          ]
                        )
                        { tctxSelf = Just t
                        }
                  def <- followType ctx tctx' templateDef
                  let subtype = typeSubtype def
                  case subtype of
                    Abstract { abstractUnderlyingType = u } ->
                      -- forward to parent
                      typeCast u
                    _ -> invalidCast
            (x, y) -> do
              t' <- unify ctx tctx x y
              x' <- unify ctx tctx x (typeClassNumeric)
              y' <- unify ctx tctx y (typeClassNumeric)
              case (t', x', y') of
                (Just _, _     , _     ) -> cast -- FIXME
                (_     , Just _, Just _) -> cast
                _                        -> invalidCast
        typeCast $ inferredType r1
