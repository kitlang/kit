module Kit.Compiler.Typers.TypeFunction where

import Control.Monad
import Data.IORef
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.Base
import Kit.Compiler.Typers.ConvertExpr
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
  -> FunctionDefinition TypedExpr ConcreteType
  -> IO (Maybe TypedDecl, Bool)
typeFunction ctx mod f = do
  debugLog ctx
    $  "typing function "
    ++ s_unpack (functionName f)
    ++ " in "
    ++ show mod
  tctx              <- modTypeContext ctx mod
  binding           <- scopeGet (modScope mod) (functionName f)
  (typed, complete) <- typeFunctionDefinition ctx tctx mod f binding
  --modifyIORef (modTypedContents mod) ((:) $ DeclFunction typed)
  return $ (Just $ DeclFunction typed, complete)

typeFunctionDefinition
  :: CompileContext
  -> TypeContext
  -> Module
  -> FunctionDefinition TypedExpr ConcreteType
  -> Binding
  -> IO (FunctionDefinition TypedExpr ConcreteType, Bool)
typeFunctionDefinition ctx tctx' mod f binding = do
  let fPos = functionPos f
  let isMain =
        (functionName f == "main") && (ctxMainModule ctx == modPath mod) && not
          (ctxIsLibrary ctx)
  functionScope <- newScope (modPath mod)
  let tctx = tctx' { tctxScopes = functionScope : tctxScopes tctx' }

  forM_
    (functionArgs f)
    (\arg -> bindToScope
      functionScope
      (argName arg)
      (newBinding
        ([], argName arg)
        (VarBinding $ newVarDefinition { varName = argName arg
                                       , varType = argType arg
                                       , varPos  = argPos arg
                                       }
        )
        (argType arg)
        []
        (argPos arg)
      )
    )
  let returnType = case (bindingConcrete binding) of
        TypeFunction rt _ _ -> rt
        _                   -> throwk $ InternalError
          "Function type was unexpectedly missing from module scope"
          Nothing
  let ftctx =
        (tctx
          { tctxScopes     = functionScope : (tctxScopes tctx)
          , tctxTypeParams = [ (paramName param, ())
                             | param <- functionParams f
                             ]
          , tctxReturnType = Just returnType
          }
        )
  body <- typeMaybeExpr ctx ftctx mod (functionBody f)
  if case body of
       Just x  -> tError x == Nothing
       Nothing -> True
    then do
      -- We're done with the body, or there wasn't one
      -- Try to unify with void; if unification doesn't fail, we didn't encounter a return statement, so the function is void.
      unification <- unify ctx ftctx mod returnType voidType
      case unification of
        TypeVarIs _ (TypeBasicType BasicTypeVoid) -> do
          resolveConstraint
            ctx
            tctx
            mod
            (TypeEq returnType
                    voidType
                    "Functions whose return type unifies with Void are Void"
                    fPos
            )
        _ -> return ()
      return
        $ ( f { functionBody      = body
              , functionType      = returnType
              , functionNamespace = (if isMain then [] else functionNamespace f)
              }
          , True
          )
    else return (f { functionBody = body }, False)
