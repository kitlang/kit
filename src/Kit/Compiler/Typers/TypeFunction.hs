module Kit.Compiler.Typers.TypeFunction where

import Control.Monad
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedStmt
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.TypeExpression
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.Log
import Kit.Parser
import Kit.Str

data ReturnTypeError = ReturnTypeError String Span Span deriving (Eq, Show)
instance Errable ReturnTypeError where
  logError reader err@(ReturnTypeError msg pos1 pos2) = do
    logErrorBasic reader err $ msg ++ ":"
    ePutStrLn $ msg
    displayFileSnippet reader pos1
    displayFileSnippet reader pos2

{-
  Type checks a non-generic function.
-}
typeFunction
  :: CompileContext
  -> TypeContext
  -> Module
  -> FunctionDefinition TypedExpr ConcreteType
  -> IO TypedStmt
typeFunction ctx tctx mod def = do
  debugLog ctx
    $  "typing function "
    ++ s_unpack (showTypePath $ functionName def)
    ++ (case functionMonomorph def of
         [] -> ""
         x  -> " monomorph " ++ show x
       )
  typed <- typeFunctionDefinition ctx tctx mod def
  return $ ps (functionPos def) $ FunctionDeclaration typed

typeFunctionDefinition
  :: CompileContext
  -> TypeContext
  -> Module
  -> FunctionDefinition TypedExpr ConcreteType
  -> IO (FunctionDefinition TypedExpr ConcreteType)
typeFunctionDefinition ctx tctx' mod f = do
  let fPos = functionPos f
  let isMain =
        (functionName f == ([], "main"))
          && (ctxMainModule ctx == modPath mod)
          && not (ctxIsLibrary ctx)
  functionScope <- newScope
  let tctx = tctx' { tctxScopes           = functionScope : tctxScopes tctx'
                   , tctxVarargsParameter = functionVararg f
                   }

  veryNoisyDebugLog ctx $ "TypeFunction: follow function args"
  args <- forM (functionArgs f) $ \arg -> do
    t <- follow ctx tctx $ argType arg
    return $ arg { argType = t }
  forM_
    args
    (\arg -> bindToScope
      functionScope
      (argName arg)
      (VarBinding $ newVarDefinition { varName = ([], argName arg)
                                     , varType = argType arg
                                     , varPos  = argPos arg
                                     }
      )
    )
  case functionVararg f of
    Just x -> do
      bindToScope
        functionScope
        x
        ( ExprBinding
        $ makeExprTyped (VarArg x) (TypeAny $ functionPos f) (functionPos f)
        )
    Nothing -> return ()
  veryNoisyDebugLog ctx $ "TypeFunction: follow function type"
  returnType <- follow ctx tctx $ functionType f
  let ftctx =
        (tctx { tctxScopes     = functionScope : (tctxScopes tctx)
              , tctxReturnType = Just returnType
              }
        )
  veryNoisyDebugLog ctx $ "TypeFunction: type function body"
  body <- typeMaybeExpr ctx ftctx mod (functionBody f)
  case body of
    Just body -> do
      let
        returnValue = exprMapReduce
          (\x -> case tExpr x of
            Return (Just x) -> Just (True, tPos x)
            Return Nothing  -> Just (False, tPos x)
            _               -> Nothing
          )
          (\val acc -> case (val, acc) of
            (_      , Left _        )                  -> acc
            (Nothing, y             )                  -> y
            (Just a , Right Nothing )                  -> Right (Just a)
            (Just a , Right (Just b)) | fst a == fst b -> acc
            (Just a , Right (Just b)) | fst a /= fst b -> Left (snd a, snd b)
          )
          tExpr
          (Right Nothing)
          body
      let unifyVoid fPos = do
            -- either no return statement, or an empty one
            unification <- unify ctx ftctx returnType TypeVoid
            case unification of
              Just _ -> do
                resolveConstraint
                  ctx
                  tctx
                  (TypeEq TypeVoid
                          returnType
                          "Functions that don't return values must be Void"
                          fPos
                  )
              _ -> return ()
      case returnValue of
        Left (a, b) -> throwk $ ReturnTypeError
          "A function can't mix return statements with and without values"
          a
          b
        Right Nothing           -> unifyVoid fPos
        Right (Just (False, p)) -> unifyVoid p
        _                       -> return ()
    Nothing -> return ()

  return $ f { functionBody = body
             , functionArgs = args
             , functionType = returnType
             }
