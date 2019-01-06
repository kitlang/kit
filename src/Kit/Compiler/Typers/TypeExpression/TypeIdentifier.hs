module Kit.Compiler.Typers.TypeExpression.TypeIdentifier (typeIdentifier) where

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
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.AutoRefDeref
import Kit.Compiler.Typers.ExprTyper
import Kit.Compiler.Typers.TypeExpression.ExprToType
import Kit.Compiler.Typers.TypeExpression.TypeVarBinding
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.Str

typeIdentifier :: SubTyper
typeIdentifier (TyperUtils { _r = r, _tryRewrite = tryRewrite, _resolve = resolve, _typeExpr = typeExpr }) ctx tctx mod ex@(TypedExpr { tExpr = et, tPos = pos })
  = case et of
    (Identifier v) -> do
      case (tctxState tctx, v) of
        (TypingPattern, Var vname) -> do
          binding <- resolveVar ctx (tctxScopes tctx) mod (tpName vname)
          case binding of
            Just binding@(EnumConstructor _) -> do
              x <- typeVarBinding ctx tctx binding pos
              return $ x
            _ -> return ex
        (TypingExprOrType, Var vname) -> do
          x <-
            (try (typeExpr ctx (tctx { tctxState = TypingExpression }) mod ex)) :: IO
              (Either KitError TypedExpr)
          case x of
            Right x   -> return x
            Left  err -> return ex
        (TypingPattern, Hole) -> do
          return ex
        (_, Var vname) -> do
          tryRewrite
            ex
            (do
              -- look for a var binding
              binding <- case fst vname of
                [] -> resolveVar ctx (tctxScopes tctx) mod (tpName vname)
                _  -> return Nothing
              binding <- case binding of
                Just _ -> return binding
                _      -> lookupBinding ctx vname
              case binding of
                Just binding -> do
                  x <- typeVarBinding ctx tctx binding pos
                  return $ case inferredType x of
                    TypeFunction _ _ _ _ -> x { tIsLvalue = True }
                    _                    -> x
                Nothing -> throwk $ TypingError
                  ("Unknown identifier: " ++ (s_unpack $ showTypePath vname))
                  pos
            )
        (_, MacroVar vname t) -> do
          case find (\(name, _) -> name == vname) (tctxMacroVars tctx) of
            Just (name, expr) ->
              typeExpr
                  ctx
                  (tctx
                    { tctxMacroVars = delete (name, expr) (tctxMacroVars tctx)
                    }
                  )
                  mod
                $ expr
            Nothing -> return ex

    (VarArgListCopy s) -> case tctxVarargsParameter tctx of
      Just x | x == s -> return ex
      Just y          -> throwk $ TypingError
        (  "`"
        ++ s_unpack s
        ++ "...` doesn't match the current variadic function; did you mean `"
        ++ s_unpack y
        ++ "`?"
        )
        pos
      Nothing -> throwk $ TypingError
        ("`" ++ s_unpack s ++ "...` can only be used in a variadic function")
        pos
