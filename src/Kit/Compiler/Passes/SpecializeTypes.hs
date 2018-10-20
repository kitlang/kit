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

{-
  When we have unresolved type variables and typing isn't making progress,
  we try to specialize type variables by looping over them and finding any
  trait specializations that satisfy the type variable's constraints. If we
  are able to specialize any, we should try typing again - the selection of a
  default type might remove the blocker and allow full typing.

  This is an attempt to unblock typing; we don't *need* to specialize all
  unresolved type variables, and we might not be able to do so, so this step
  shouldn't cause a failure.
-}
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

{-
  Template variables generate a new type variable for each combination of
  generic parameters for a given generic. Sometimes we have only partial
  parameters, so we may create multiple type variables that eventually end up
  pointing to the same monomorph - for example, we may have List[Int] and
  List[Unknown 1] and later unify Unknown 1 with Int. When this happens, we
  need to unify all of the type variables from both monomorphs' template
  variables to infer their types and make sure they're consistent.
-}
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
