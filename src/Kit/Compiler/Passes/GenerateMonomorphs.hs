module Kit.Compiler.Passes.GenerateMonomorphs where

import Control.Monad
import Data.Either
import Data.Function
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Error
import Kit.HashTable
import Kit.Parser

constantScopes
  :: CompileContext
  -> TypeContext
  -> [TypeParam]
  -> [ConcreteType]
  -> Span
  -> IO [Scope Binding]
constantScopes ctx tctx params paramValues pos = do
  let consts =
        [ (param, ct)
        | (param, ct) <- zip params paramValues
        , typeParamIsConstant param
        ]
  scopes <- if null consts
    then return $ tctxScopes tctx
    else do
      scope <- newScope
      forMWithErrors_ consts $ \(param, ct) -> do
        case ct of
          ConstantType v -> do
            tv <- makeTypeVar ctx pos
            bindToScope scope
                        (paramName param)
                        (ExprBinding $ makeExprTyped (Literal v tv) tv pos)
          x -> throwk $ TypingError
            ("Invalid value for constant type parameter: " ++ show x)
            pos
      return $ scope : tctxScopes tctx
  return scopes

generateMonomorphs :: CompileContext -> IO [(Module, TypedDeclWithContext)]
generateMonomorphs ctx = do
  pendingGenerics <- readIORef (ctxPendingGenerics ctx)
  results <- forM (reverse pendingGenerics) $ \(tp@(modPath, name), params') ->
    do
      mod        <- getMod ctx modPath
      tctx       <- modTypeContext ctx mod
      params     <- forM params' (mapType $ follow ctx tctx)
      unresolved <- forM params $ typeUnresolved ctx tctx
      if or unresolved
        -- don't try to generate a monomorph if the params contain unresolved
        -- type variables; if we needed this monomorph and don't know its param
        -- values, it'll blow up elsewhere
        then return $ Just $ Left (tp, params')
        else do
          existing <- h_lookup (ctxCompleteGenerics ctx) (tp, params)
          case existing of
            Just x -> return Nothing
            _      -> do
              h_insert (ctxCompleteGenerics ctx) (tp, params) ()
              binding <- getBinding ctx tp
              case binding of
                FunctionBinding def -> do
                  let monoTctx = addTypeParams
                        tctx
                        [ (functionSubPath def $ paramName param, ct)
                        | (param, ct) <- zip (functionParams def) params
                        ]
                  scopes <- constantScopes ctx
                                           monoTctx
                                           (functionParams def)
                                           params
                                           (functionPos def)
                  return $ Just $ Right
                    ( mod
                    , ( DeclFunction $ def { functionMonomorph = params }
                      , monoTctx { tctxScopes = scopes }
                      )
                    )

                TypeBinding def -> do
                  let thisType = TypeInstance (typeName def) params
                  let monoTctx =
                        (addTypeParams
                          tctx
                          [ (typeSubPath def $ paramName param, ct)
                          | (param, ct) <- zip (typeParams def) params
                          ]
                        )
                  scopes <- constantScopes ctx
                                           monoTctx
                                           (typeParams def)
                                           params
                                           (typePos def)
                  return
                    $ Just
                    $ Right
                    $ ( mod
                      , ( DeclType $ def { typeMonomorph = params }
                        , monoTctx { tctxScopes = scopes }
                        )
                      )

                TraitBinding def -> do
                  let thisType = TypeBox (traitName def) params
                  let monoTctx = addTypeParams
                        tctx
                        [ (traitSubPath def $ paramName param, ct)
                        | (param, ct) <- zip (traitAllParams def) params
                        ]
                  scopes <- constantScopes ctx
                                           monoTctx
                                           (traitParams def)
                                           params
                                           (traitPos def)
                  return $ Just $ Right
                    ( mod
                    , ( DeclTrait $ def { traitMonomorph = params }
                      , monoTctx { tctxScopes = scopes }
                      )
                    )

                _ -> return Nothing

  let realResults = catMaybes results
  let unresolved  = lefts realResults
  let decls       = rights realResults

  writeIORef (ctxPendingGenerics ctx) unresolved
  return decls
