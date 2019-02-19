module Kit.Compiler.Passes.FlattenTraits (flattenTraits) where

import Control.Monad
import Data.Mutable
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.NameMangling
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.Typers.ConvertExpr
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser
import Kit.Str

flattenTraits
  :: CompileContext
  -> [(Module, [TypedStmtWithContext])]
  -> IO [(Module, [TypedStmtWithContext])]
flattenTraits ctx contents = do
  memos <- h_new
  forM contents $ \(mod, contents) -> do
    contents <- forM contents $ \(decl, tctx) -> case stmt decl of
      Implement impl@(TraitImplementation { implTrait = TypeTraitConstraint trait, implFor = TypeTraitConstraint (tpFor, paramsFor) })
        -> do
        -- implement trait for trait; find concrete implementors of the for trait
          allChildren <- findImpls ctx memos (tpFor, paramsFor)
          impls       <- forM allChildren $ \ct -> do
            impl     <- return $ impl { implFor = ct }
            existing <- h_lookup (ctxImpls ctx) trait
            impls    <- case existing of
              Just x  -> return x
              Nothing -> do
                impls <- h_new
                h_insert (ctxImpls ctx) trait impls
                return impls
            h_insert impls ct impl
            return (ps (implPos impl) $ Implement $ impl { implFor = ct }, tctx)
          return $ Just impls

      _ -> return $ Just [(decl, tctx)]
    return (mod, foldr (++) [] $ catMaybes contents)

findImpls
  :: CompileContext
  -> HashTable TraitConstraint [ConcreteType]
  -> TraitConstraint
  -> IO [ConcreteType]
findImpls ctx memos t = do
  memoized <- h_lookup memos t
  case memoized of
    Just x  -> return x
    Nothing -> do
      directs <- h_lookup (ctxImpls ctx) t
      case directs of
        Just x -> do
          childList <- h_toList x
          impls     <- forMWithErrors childList $ \(ct, _) -> do
            case ct of
              TypeTraitConstraint tp -> findImpls ctx memos tp
              _                      -> return [ct]
          impls <- return $ foldr (++) [] impls
          h_insert memos t impls
          return impls
        Nothing -> return []
