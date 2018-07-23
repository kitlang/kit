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

  let fPos = case functionBody f of
        Just x  -> pos x
        Nothing -> null_span

  let isMain = (functionName f == "main") && (ctxMainModule ctx == modPath mod) && not (ctxIsLibrary ctx)
  imports       <- getModImports ctx mod
  functionScope <- newScope
  tctx          <- newTypeContext (map modScope imports)
  args          <- forM
    (functionArgs f)
    (\arg -> do
      -- FIXME: position
      argType    <- resolveMaybeType ctx tctx mod fPos (argType arg)
      argDefault <- maybeConvert (typeExpr ctx tctx mod) (argDefault arg)
      return $ newArgSpec { argName    = argName arg
                          , argType    = argType
                          , argDefault = argDefault
                          }
    )

  forM_
    args
    (\arg -> bindToScope functionScope
                         (argName arg)
                          -- FIXME: arg position
                         (newBinding VarBinding (argType arg) Nothing fPos)
    )
  returnType <- resolveMaybeType ctx tctx mod fPos (functionType f)
  ct         <- scopeGet (modScope mod) (functionName f)
  let concreteRt = case (bindingConcrete ct) of
        TypeFunction rt _ _ -> rt
        _                   -> throwk $ InternalError
          "Function type was unexpectedly missing from module scope"
          Nothing
  resolveConstraint
    ctx
    tctx
    mod
    (TypeEq concreteRt
            returnType
            "Function return type and type annotation must match"
            (fPos)
    )
  let functionTypeContext =
        (tctx { tctxScopes     = functionScope : (tctxScopes tctx)
              , tctxReturnType = Just returnType
              }
        )
  converted <- convertFunctionDefinition
    (typeExpr ctx functionTypeContext mod)
    (resolveMaybeType ctx tctx mod fPos)
    args
    returnType
    f
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
                fPos
        )
      knownType ctx tctx mod resolvedReturnType
    _ -> return resolvedReturnType
  let typedFunction = converted
        { functionType         = finalReturn
        , functionNameMangling = (if isMain
                                   then Nothing
                                   else functionNameMangling f
                                 )
        }
  bindToScope (modTypedContents mod)
              (functionName f)
              (TypedFunctionDecl typedFunction)
