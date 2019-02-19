module Kit.Compiler.Typers.TypeExpression.TypeMatch (typeMatch) where

import Control.Applicative
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
import Kit.Compiler.Typers.AutoRefDeref
import Kit.Compiler.Typers.ExprTyper
import Kit.Compiler.Typers.TypeExpression.ExprToType
import Kit.Compiler.Typers.TypeExpression.TypeVarBinding
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.Str

typeMatch :: SubTyper
typeMatch (TyperUtils { _r = r, _tryRewrite = tryRewrite, _resolve = resolve, _typeExpr = typeExpr, _maybeR = maybeR }) ctx tctx mod ex@(TypedExpr { tExpr = et, tPos = pos })
  = case et of
    (Match e1 cases (e2)) -> do
      r1 <- r e1
      r1 <- return $ if tIsLvalue r1 then r1 else makeLvalue r1
      r2 <- maybeR e2
      let tctx' = tctx { tctxState = TypingPattern }
      cases' <- forMWithErrors cases $ \c -> do
        let tctx' = tctx { tctxState = TypingPattern }
        pattern <- typeExpr ctx tctx' mod $ matchPattern c
        resolve $ TypeEq
          (inferredType r1)
          (inferredType pattern)
          "Match pattern must match the type of the matched value"
          (tPos pattern)
        patternScope <- newScope
        let ids = exprMapReduce
              (\x -> case tExpr x of
                Identifier (Var v) -> Just (v, inferredType x, tPos x)
                _                  -> Nothing
              )
              (\x acc -> case x of
                Just x  -> x : acc
                Nothing -> acc
              )
              tExpr
              []
              pattern
        forMWithErrors_ ids $ \(id, t, pos) -> do
          bindToScope patternScope (tpName id)
            $ VarBinding
                (newVarDefinition { varName    = id
                                  , varType    = t
                                  , varIsConst = True
                                  }
                )
        let tctx' = tctx { tctxScopes = patternScope : (tctxScopes tctx) }
        body <- typeExpr ctx tctx' mod $ makeBlock $ matchBody c
        return $ MatchCase {matchPattern = pattern, matchBody = body}
      return $ makeExprTyped (Match r1 cases' r2) (TypeVoid) pos
