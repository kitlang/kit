module Kit.Compiler.Typers.TypeExpression.TypeVarDeclaration where

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

typeVarDeclaration :: SubTyper
typeVarDeclaration (TyperUtils { _r = r, _tryRewrite = tryRewrite, _resolve = resolve, _typeExpr = typeExpr, _maybeR = maybeR }) ctx tctx mod ex@(TypedExpr { tExpr = et, tPos = pos })
  = case et of
    (VarDeclaration s@(MacroVar vname _) a const b) -> do
      case find (\(name, _) -> name == vname) (tctxMacroVars tctx) of
        Just (_, x@TypedExpr { tExpr = Identifier v@(Var vname) }) ->
          r $ makeExprTyped (VarDeclaration v a const b) (inferredType ex) pos
        x -> throwk $ InternalError
          (  "Macro var $"
          ++ s_unpack vname
          ++ " isn't bound to an identifier: "
          ++ show x
          )
          (Just pos)

    (VarDeclaration (Var vname) _ const init) -> do
      when (const && isNothing init) $ throwk $ TypingError
        ("const must have an initial value")
        pos

      let
        typeVarDec = do
          varType <- makeGenericConcrete ctx pos $ inferredType ex
          varType <- follow ctx tctx varType
          init'   <- case init of
            Just e1 -> do
              r1        <- r e1
              converted <- tryAutoRefDeref ctx tctx varType r1
              resolve $ TypeEq
                varType
                (inferredType converted)
                "A variable's initial value must match the variable's type"
                (tPos r1)
              return $ Just converted
            Nothing -> return Nothing
          existing <- resolveLocal (head $ tctxScopes tctx) (tpName vname)
          case existing of
            Just x -> throwk $ DuplicateDeclarationError (modPath mod)
                                                         (tpName vname)
                                                         pos
                                                         (bindingPos x)
            _ -> bindToScope
              (head $ tctxScopes tctx)
              (tpName vname)
              (VarBinding
                (newVarDefinition { varName    = vname
                                  , varType    = varType
                                  , varDefault = init'
                                  , varPos     = pos
                                  , varIsLocal = True
                                  , varIsConst = const
                                  }
                )
              )
          return $ makeExprTyped
            (VarDeclaration (Var vname) (varType) const init')
            varType
            pos

      result <- (try $ typeVarDec) :: IO (Either KitError TypedExpr)
      case result of
        Left err -> do
          -- leave a dummy binding in the scope; since we're going to fail
          -- anyway, we may be able to get more type info and will avoid
          -- spurious "unknown identifier" errors downstream
          bindToScope
            (head $ tctxScopes tctx)
            (tpName vname)
            (VarBinding
              (newVarDefinition { varName    = vname
                                , varType    = inferredType ex
                                , varDefault = Nothing
                                , varPos     = pos
                                }
              )
            )
          throwk err
        Right x -> return x

    -- (Defer e1) -> do
    --   throwk $ InternalError "Not yet implemented" (Just pos)
