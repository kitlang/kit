module Kit.Compiler.Passes.GenerateMonomorphs
  ( generateMonomorphs
  )
where

import           Control.Monad
import           Data.Either
import           Data.Function
import           Data.Mutable
import           Data.List
import           Data.Maybe
import           Kit.Ast
import           Kit.Compiler.Binding
import           Kit.Compiler.Context
import           Kit.Compiler.Module
import           Kit.Compiler.Scope
import           Kit.Compiler.TypeContext
import           Kit.Error
import           Kit.HashTable
import           Kit.Parser

generateMonomorphs :: CompileContext -> IO [(Module, TypedStmtWithContext)]
generateMonomorphs ctx = do
  pendingGenerics <- readRef (ctxPendingGenerics ctx)
  results         <- forM pendingGenerics $ \(tp@(modPath, name), params') -> do
    mod    <- getMod ctx modPath
    tctx   <- modTypeContext ctx mod
    params <- forM params' $ follow ctx tctx
    let unresolved = map (mapType_ typeUnresolved) params
    if or (foldr (++) [] unresolved)
      -- don't try to generate a monomorph if the params contain unresolved
      -- type variables; if we needed this monomorph and don't know its param
      -- values, it'll blow up elsewhere
      then return $ Just $ Left (tp, params')
      else do
        existing <- h_lookup (ctxCompleteGenerics ctx) (tp, params)
        case existing of
          Just True -> return Nothing
          _      -> do
            h_insert (ctxCompleteGenerics ctx) (tp, params) True
            binding <- getBinding ctx tp
            case binding of
              FunctionBinding def -> do
                monoTctx <- addTypeParams
                  ctx
                  tctx
                  [ (functionSubPath def $ paramName param, ct)
                  | (param, ct) <- zip (functionParams def) params
                  ]
                  (functionPos def)
                return $ Just $ Right
                  ( mod
                  , ( ps (functionPos def) $ FunctionDeclaration $ def
                      { functionMonomorph = params
                      }
                    , monoTctx
                    )
                  )

              TypeBinding def -> do
                let thisType = TypeInstance (typeName def) params
                monoTctx <- addTypeParams
                  ctx
                  tctx
                  [ (typeSubPath def $ paramName param, ct)
                  | (param, ct) <- zip (typeParams def) params
                  ]
                  (typePos def)
                return $ if hasMeta "builtin" (typeMeta def)
                  then Nothing
                  else
                    Just
                    $ Right
                    $ ( mod
                      , ( ps (typePos def) $ TypeDeclaration $ def
                          { typeMonomorph = params
                          }
                        , monoTctx
                        )
                      )

              TraitBinding def -> do
                let thisType = TypeBox (traitName def) params
                monoTctx <- addTypeParams
                  ctx
                  tctx
                  [ (traitSubPath def $ paramName param, ct)
                  | (param, ct) <- zip (traitAllParams def) params
                  ]
                  (traitPos def)
                return $ Just $ Right
                  ( mod
                  , ( ps (traitPos def) $ TraitDeclaration $ def
                      { traitMonomorph = params
                      }
                    , monoTctx
                    )
                  )

              _ -> return Nothing

  let realResults = catMaybes results
  let unresolved  = lefts realResults
  let decls       = rights realResults

  writeRef (ctxPendingGenerics ctx) unresolved
  return decls
