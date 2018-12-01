module Kit.Compiler.Typers.TypeExpression.TypeControl where

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
import Kit.Compiler.Typers.TypeExpression.TypeVarBinding
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.Str

typeControl :: SubTyper
typeControl (TyperUtils { _r = r, _tryRewrite = tryRewrite, _resolve = resolve, _typeExpr = typeExpr }) ctx tctx mod ex@(TypedExpr { tExpr = et, tPos = pos })
  = case et of
    (For e1@(TypedExpr { tExpr = Identifier (Var id) }) e2 e3) -> do
      r2 <- r e2
      tv <- follow ctx tctx $ inferredType e1
      case tExpr r2 of
        RangeLiteral eFrom eTo -> do
          forMWithErrors_ [eFrom, eTo] $ \x -> resolve $ TypeEq
            tv
            (inferredType x)
            "For identifier must match the iterator's type"
            (tPos x)

          scope <- newScope
          bindToScope scope (tpName id)
            $ VarBinding
                (newVarDefinition { varName    = id
                                  , varType    = tv
                                  , varIsConst = True
                                  }
                )
          r3 <- typeExpr
            ctx
            (tctx { tctxScopes    = scope : (tctxScopes tctx)
                  , tctxLoopCount = (tctxLoopCount tctx) + 1
                  }
            )
            mod
            e3

          return $ makeExprTyped (For (e1 { inferredType = tv }) r2 r3)
                                 TypeVoid
                                 pos

        _ -> do
          tryRewrite (makeExprTyped (For e1 r2 e3) TypeVoid pos) $ do
            let
              tryIterable = do
                -- try to convert to an Iterable
                let
                  fail = throwk $ TypingError
                    "For statements must iterate over a supported type, such as `Iterator` or `Iterable`, with a `for` rewrite rule"
                    pos
                box <- autoRefDeref ctx
                                    tctx
                                    (TypeBox typeClassIterablePath [])
                                    r2
                case box of
                  Just box -> do
                    box <- r box
                    tryRewrite (makeExprTyped (For e1 box e3) TypeVoid pos)
                      $ fail
                  Nothing -> fail
            -- try to convert to an Iterator
            box <- autoRefDeref ctx tctx (TypeBox typeClassIteratorPath [tv]) r2
            case box of
              Just box -> do
                box <- r box
                tryRewrite (makeExprTyped (For e1 box e3) TypeVoid pos)
                  $ tryIterable
              Nothing -> tryIterable

    (For e1 e2 e3) -> do
      r1 <- typeExpr ctx (tctx { tctxState = TypingPattern }) mod e1
      case tExpr r1 of
        Identifier (Var _) -> r $ makeExprTyped (For r1 e2 e3) TypeVoid pos
        _ -> throwk $ TypingError ("Invalid for loop iterable") (tPos r1)

    (While e1 e2 d) -> do
      r1    <- r e1
      scope <- newScope
      r2    <- typeExpr
        ctx
        (tctx { tctxScopes    = scope : (tctxScopes tctx)
              , tctxLoopCount = (tctxLoopCount tctx) + 1
              }
        )
        mod
        e2
      resolve $ TypeEq (inferredType r1)
                       (TypeBool)
                       "A while condition must be a Bool"
                       (tPos r1)
      return $ makeExprTyped (While r1 r2 d) TypeVoid pos

    (If e1 e2 (Just e3)) -> do
      r1       <- r e1
      scope1   <- newScope
      [r2, r3] <- forMWithErrors [e2, e3] $ \e -> do
        scope <- newScope
        typeExpr ctx (tctx { tctxScopes = scope : (tctxScopes tctx) }) mod e
      tv <- makeTypeVar ctx pos
      resolve $ TypeEq (inferredType r1)
                       (TypeBool)
                       "An if condition must be a Bool"
                       (tPos r1)
      resolve $ TypeEq
        (inferredType r2)
        (inferredType r3)
        "In an if expression with an else clause, both clauses must have the same type"
        pos
      resolve $ TypeEq (inferredType r2)
                       (tv)
                       "The type of an if expression must match its clauses"
                       (tPos r2)
      return $ makeExprTyped (If r1 r2 (Just r3)) (tv) pos

    (If e1 e2 Nothing) -> do
      r1    <- r e1
      -- FIXME: scope
      scope <- newScope
      r2    <- typeExpr ctx
                        (tctx { tctxScopes = scope : (tctxScopes tctx) })
                        mod
                        e2
      resolve $ TypeEq (inferredType r1)
                       (TypeBool)
                       "An if condition must be a Bool"
                       (tPos r1)
      return $ makeExprTyped (If r1 r2 Nothing) TypeVoid pos

    (Continue) -> do
      if (tctxLoopCount tctx > 0)
        then return $ makeExprTyped (Continue) TypeVoid pos
        else throwk $ TypingError "Can't `continue` outside of a loop" pos

    (Break) -> do
      if (tctxLoopCount tctx > 0)
        then return $ makeExprTyped (Break) TypeVoid pos
        else throwk $ TypingError "Can't `break` outside of a loop" pos
