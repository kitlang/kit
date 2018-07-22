module Kit.Compiler.Typers.TypeFunction where

import Control.Monad
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.Typers.Base
import Kit.Compiler.Typers.TypeExpression
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Parser
import Kit.Str

typeFunction
  :: CompileContext
  -> Module
  -> FunctionDefinition Expr (Maybe TypeSpec)
  -> IO ()
typeFunction ctx mod f = do
  debugLog ctx
    $  "typing function "
    ++ s_unpack (functionName f)
    ++ " in "
    ++ show mod
  case functionBody f of
    Just x -> do
      let isMain =
            (functionName f == "main") && (modPath mod == ctxMainModule ctx)
      imports       <- getModImports ctx mod
      functionScope <- newScope
      tctx          <- newTypeContext (map modScope imports)
      args          <- mapM
        (\arg -> do
          argType <- resolveMaybeType ctx tctx mod (pos x) (argType arg)
          return $ newArgSpec { argName = argName arg, argType = argType }
        )
        (functionArgs f)
      forM_
        args
        (\arg -> bindToScope
          functionScope
          (argName arg)
                             -- FIXME: arg position
          (newBinding VarBinding (argType arg) Nothing (pos x))
        )
      returnType <- resolveMaybeType ctx tctx mod (pos x) (functionType f)
      ct         <- scopeGet (modScope mod) (functionName f)
      let concreteRt = case (bindingConcrete ct) of
            TypeFunction rt _ _ -> rt
            _                   -> throwk $ InternalError
              "Function type was unexpectedly missing from module scope"
              (Just $ pos x)
      resolveConstraint
        ctx
        tctx
        mod
        (TypeEq concreteRt
                returnType
                "Function return type and type annotation must match"
                (pos x)
        )
      typedBody <- typeExpr
        ctx
        (tctx { tctxScopes     = functionScope : (tctxScopes tctx)
              , tctxReturnType = Just returnType
              }
        )
        mod
        x

      resolvedReturnType <- knownType ctx tctx mod returnType
      -- Try to unify with void; if unification doesn't fail, we didn't encounter a return statement, so the function is void.
      unification        <- unify ctx tctx mod (resolvedReturnType) voidType
      finalReturn        <- case unification of
        TypeVarIs _ (TypeBasicType BasicTypeVoid) -> do
          resolveConstraint
            ctx
            tctx
            mod
            (TypeEq resolvedReturnType
                    voidType
                    "Functions whose return type unifies with Void are Void"
                    (pos x)
            )
          knownType ctx tctx mod resolvedReturnType
        _ -> return resolvedReturnType
      let
        typedFunction = ((convertFunctionDefinition f) :: TypedFunction)
          { functionName         = functionName f
          , functionType         = finalReturn
          , functionArgs         = args
          , functionBody         = Just typedBody
          , functionVarargs      = (functionVarargs f)
          , functionNameMangling = (if isMain
                                     then Nothing
                                     else functionNameMangling f
                                   )
          }
      bindToScope (modTypedContents mod)
                  (functionName f)
                  (TypedFunction typedFunction)
    _ -> return ()
