module Kit.Compiler.Passes.SpecializeTypes where

import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.TypeContext
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.NameMangling
import Kit.Str

specializeTypes :: CompileContext -> IO Bool
specializeTypes ctx = do
  typeVars     <- specializeTypeVars ctx
  templateVars <- unifyTemplateVars ctx
  return $ typeVars || templateVars

specializeTypeVars ctx = do
  unresolved          <- readIORef (ctxUnresolvedTypeVars ctx)
  (result, remaining) <- foldM
    (\(result, remaining) id -> do
      -- for type variables with constraints and no value, try to specialize
      defaultType <- findDefaultType ctx id
      case defaultType of
        Just x -> do
          noisyDebugLog ctx
            $  "specializing type variable "
            ++ show id
            ++ " as "
            ++ show x
          info <- getTypeVar ctx id
          h_insert (ctxTypeVariables ctx) id (info { typeVarValue = Just x })
          return (True, remaining)
        _ -> return (result, id : remaining)
    )
    (False, [])
    unresolved
  writeIORef (ctxUnresolvedTypeVars ctx) remaining
  return result

findDefaultType :: CompileContext -> Int -> IO (Maybe ConcreteType)
findDefaultType ctx id = do
  info <- getTypeVar ctx id
  case typeVarValue info of
    Just x  -> return (Just x)
    Nothing -> do
      if null (typeVarConstraints info)
        then return Nothing
        else do
          tctx <- newTypeContext []
          let constraints = typeVarConstraints info
          defaults <- mapM (h_lookup (ctxTraitSpecializations ctx))
                           (map (fst . fst) constraints)
          let specializations = catMaybes defaults
          specialization <- foldM
            -- FIXME: we should be storing specializations as ConcreteTypes, not TypeSpecs
            (\acc (ct, _) -> do
              case acc of
                Just _  -> return acc
                Nothing -> do
                  meetConstraints <- foldM
                    (\acc' c -> case acc' of
                      Just _ -> do
                        -- FIXME: params
                        result <- unify ctx
                                        tctx
                                        ct
                                        (TypeTraitConstraint (c, []))
                        return $ case result of
                          Just _ -> acc'
                          _      -> Nothing
                      Nothing -> do
                        return acc'
                    )
                    (Just ct)
                    (map (fst . fst) constraints)
                  case meetConstraints of
                    Just _  -> return meetConstraints
                    Nothing -> return Nothing
            )
            Nothing
            specializations
          case specialization of
            Just t -> do
              return $ Just t
            _ -> throwk $ BasicError
              ("This expression has constraints: \n\n"
              ++ (intercalate
                   "\n"
                   [ "  - "
                     ++ s_unpack (showTypePath c)
                     ++ " ("
                     ++ reason
                     ++ ")"
                   | ((c, _), (reason, _)) <- constraints
                   ]
                 )
              ++ "\n\nbut no specialization for one of these traits satisfies all of them, so no concrete type can be determined.\n\nTry adding a type annotation: `(myExpression: Type)`"
              )
              (Just $ head $ typeVarPositions info)

unifyTemplateVars :: CompileContext -> IO Bool
unifyTemplateVars ctx = do
  unresolved          <- readIORef (ctxUnresolvedTemplateVars ctx)
  (result, remaining) <- foldM
    (\(result, remaining) (id, params, pos) -> do
      -- if we can fully resolve these type parameters, unify with the fully
      -- resolved version of the monomorph
      tctx           <- newTypeContext []
      resolvedParams <- forM params $ mapType $ follow ctx tctx
      let unresolved = map typeUnresolved resolvedParams
      if or unresolved
        then return (result, (id, params, pos) : remaining)
        else do
          let oldKey = hashParams params
          let newKey = hashParams resolvedParams
          if oldKey == newKey
            then return (result, remaining)
            else do
              monos   <- h_get (ctxTemplateVariables ctx) id
              oldMono <- h_get monos oldKey
              newMono <- h_lookup monos newKey
              case newMono of
                Just newMono -> resolveConstraint
                  ctx
                  tctx
                  (TypeEq (TypeTypeVar newMono)
                          (TypeTypeVar oldMono)
                          "Inferred types of equivalent monomorphs must match"
                          (pos)
                  )
                Nothing -> h_insert monos newKey oldMono
              return (True, remaining)
    )
    (False, [])
    unresolved
  writeIORef (ctxUnresolvedTemplateVars ctx) remaining
  return $ result || (not $ null remaining)
