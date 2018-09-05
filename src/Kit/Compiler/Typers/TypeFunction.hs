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
import Kit.Compiler.Typers.TypeExpression
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser
import Kit.Str

data ReturnTypeError = ReturnTypeError String Span Span deriving (Eq, Show)
instance Errable ReturnTypeError where
  logError err@(ReturnTypeError msg pos1 pos2) = do
    logErrorBasic err $ msg ++ ":"
    ePutStrLn $ msg
    displayFileSnippet pos1
    displayFileSnippet pos2

{-
  Type checks a non-generic function.
-}
typeFunction
  :: CompileContext
  -> Module
  -> FunctionDefinition TypedExpr ConcreteType
  -> IO (Maybe TypedDecl)
typeFunction ctx mod f = do
  debugLog ctx $ "typing function " ++ s_unpack (showTypePath $ functionName f)
  tctx  <- modTypeContext ctx mod
  typed <- typeFunctionDefinition ctx tctx mod f
  return $ Just $ DeclFunction typed

{-
  Type checks a specific monomorph of a generic function, with a known set of
  parameters. By this point the final types of the parameters must be known.
-}
typeFunctionMonomorph
  :: CompileContext
  -> Module
  -> FunctionDefinition TypedExpr ConcreteType
  -> [ConcreteType]
  -> IO (Maybe TypedDecl)
typeFunctionMonomorph ctx mod f params = do
  debugLog ctx
    $  "generating function monomorph for "
    ++ s_unpack (showTypePath $ functionName f)
    ++ " with params "
    ++ show params
    ++ " in "
    ++ show mod
  tctx <- modTypeContext ctx mod
  let tctx' = tctx
        { tctxTypeParams = [ (functionSubPath f $ paramName param, ct)
                           | (param, ct) <- zip (functionParams f) params
                           ]
        }
  typed <- typeFunctionDefinition ctx tctx' mod f
  return $ Just $ DeclFunction typed

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
  functionScope <- newScope (modPath mod)
  let tctx = tctx' { tctxScopes = functionScope : tctxScopes tctx' }

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
  returnType <- follow ctx tctx $ functionType f
  let ftctx =
        (tctx { tctxScopes     = functionScope : (tctxScopes tctx)
              , tctxReturnType = Just returnType
              }
        )
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
            unification <- unify ctx ftctx returnType voidType
            case unification of
              Just _ -> do
                resolveConstraint
                  ctx
                  tctx
                  (TypeEq voidType
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

