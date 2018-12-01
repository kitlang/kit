module Kit.Compiler.Typers.TypeExpression.TypeArrayAccess where

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

typeArrayAccess :: SubTyper
typeArrayAccess (TyperUtils { _r = r, _tryRewrite = tryRewrite, _resolve = resolve, _typeExpr = typeExpr }) ctx tctx mod ex@(TypedExpr { tExpr = et, tPos = pos })
  = case et of
    (ArrayAccess e1 e2) -> do
      r1 <- r e1
      r2 <- typeExpr ctx (tctx { tctxState = TypingExprOrType }) mod e2
      tryRewrite (makeExprTyped (ArrayAccess r1 r2) (inferredType ex) pos) $ do
        let fail = throwk $ TypingError
              (  "Array access is not supported on values of type "
              ++ show (inferredType r1)
              )
              pos
        let
          resolveArrayAccess t = case (t, tExpr r2) of
            (TypeTuple t, Literal (IntValue i) _) ->
              -- FIXME: this should work with any constant Int expression
              -- compile-time tuple slot access
              if (i >= 0) && (i < length t)
                then r
                  (r1 { tExpr        = TupleSlot r1 i
                      , inferredType = t !! i
                      , tPos         = tPos r1 <+> tPos e2
                      }
                  )
                else throwk $ TypingError
                  (  "Access to invalid tuple slot (tuple has "
                  ++ show (length t)
                  ++ " slots)"
                  )
                  pos

            (TypeTuple t, _) -> throwk $ TypingError
              "Array access on tuples is only allowed using int literals"
              pos

            (TypeArray t _, _) -> do
              resolveConstraint
                ctx
                tctx
                (TypeEq t
                        (inferredType ex)
                        "Array access on an array will return the inner type"
                        (tPos ex)
                )
              resolveConstraint
                ctx
                tctx
                (TypeEq
                  typeClassIntegral
                  (inferredType r2)
                  "Array access on an array requires an Integral argument"
                  (tPos r2)
                )
              return $ (makeExprTyped (ArrayAccess r1 r2) t pos) { tIsLvalue = True
                                                                 }

            (TypePtr t, _) -> do
              resolveConstraint
                ctx
                tctx
                (TypeEq
                  t
                  (inferredType ex)
                  "Array access on a pointer will dereference the pointer"
                  (tPos ex)
                )
              resolveConstraint
                ctx
                tctx
                (TypeEq
                  typeClassIntegral
                  (inferredType r2)
                  "Array access on a pointer requires an Integral argument"
                  (tPos r2)
                )
              return $ makeExprTyped (ArrayAccess r1 r2) (inferredType ex) pos

            (TypeInstance tp params, _) -> do
              def <- getTypeDefinition ctx tp
              case typeSubtype def of
                Abstract { abstractUnderlyingType = t } -> do
                  tctx <- genericTctx ctx tctx pos (TypeInstance tp params)
                  t    <- follow ctx tctx t
                  resolveArrayAccess t
                _ -> fail

            (TypeTypeOf tp params, _) -> do
              case tCompileTimeValue r2 of
                Just v -> return $ r1
                  { inferredType = TypeTypeOf tp (params ++ [ConstantType v])
                  }
                _ -> do
                  t <- exprToType ctx tctx mod (tPos r2) r2
                  case t of
                    Just t -> return
                      $ r1 { inferredType = TypeTypeOf tp (params ++ [t]) }
                    Nothing -> throwk $ TypingError
                      (  "Unknown type parameter value: "
                      ++ show (inferredType r2)
                      )
                      pos

            (TypeTraitConstraint (tp, params), _) -> do
              case tCompileTimeValue r2 of
                Just v -> return $ r1
                  { inferredType = TypeTraitConstraint
                    (tp, params ++ [ConstantType v])
                  }
                _ -> do
                  t <- exprToType ctx tctx mod (tPos r2) r2
                  case t of
                    Just t -> return $ r1
                      { inferredType = TypeTraitConstraint (tp, params ++ [t])
                      }
                    Nothing -> throwk $ TypingError
                      (  "Unknown type parameter value: "
                      ++ show (inferredType r2)
                      )
                      pos

            _ -> fail

        resolveArrayAccess $ inferredType r1
