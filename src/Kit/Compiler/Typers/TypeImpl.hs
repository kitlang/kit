module Kit.Compiler.Typers.TypeImpl where

import Control.Monad
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedStmt
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.TypeFunction
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.Str

{-
  Type checks a trait implementation.
-}
typeImpl
  :: CompileContext
  -> TypeContext
  -> Module
  -> TraitImplementation TypedExpr ConcreteType
  -> IO TypedStmt
typeImpl ctx tctx' mod def = do
  (traitDef, params, tctx) <- case implTrait def of
    TypeTraitConstraint (tp, params) -> do
      traitDef <- getTraitDefinition ctx tp
      params   <- makeGeneric ctx tp (implPos def) params
      tctx     <- genericTctx ctx
                              tctx'
                              (implPos def)
                              (TypeTraitConstraint (tp, map snd params))
      params <- forMWithErrors (map snd params) $ mapType $ follow ctx tctx
      return (traitDef, params, tctx { tctxThis = Just $ implFor def })
    _ -> throwk $ TypingError
      ("Couldn't find trait " ++ show (implTrait def))
      (implPos def)

  debugLog ctx
    $  "typing trait implemention of "
    ++ s_unpack (showTypePath $ traitName traitDef)
    ++ " for "
    ++ show (implFor def)

  [methods, staticMethods] <-
    forMWithErrors
        [(traitMethods, implMethods), (traitStaticMethods, implStaticMethods)]
      $ \(traitMethods, implMethods) -> do
          methods <- forMWithErrors (traitMethods traitDef) $ \method ->
            case
                findMethodByName (implMethods def)
                                 (tpName $ functionName method)
              of
                Just x  -> return x
                Nothing -> case functionBody method of
                  Just x  -> return method
                  Nothing -> throwk $ TypingError
                    (  "Trait implementation of "
                    ++ s_unpack (showTypePath $ traitName traitDef)
                    ++ " for "
                    ++ show (implFor def)
                    ++ " is missing method "
                    ++ s_unpack (tpName $ functionName method)
                    )
                    (implPos def)

          methods <- forMWithErrors
            methods
            (\method -> do
              typed <- typeFunctionDefinition ctx tctx mod method
              case
                  findMethodByName (traitMethods traitDef)
                                   (tpName $ functionName method)
                of
                  Just traitMethod -> do
                    traitMethod <- followFunction ctx tctx traitMethod
                    mergeFunctionInfo
                      ctx
                      tctx'
                      traitMethod
                      method
                      "Trait implementation method return type must match the trait's definition"
                      "Trait implementation method argument type must match the trait's definition"
                  Nothing -> return ()
              return typed
            )
          return methods

  return $ ps (implPos def) $ Implement $ def
    { implMethods       = methods
    , implStaticMethods = staticMethods
    }

findMethodByName
  :: [FunctionDefinition TypedExpr ConcreteType]
  -> Str
  -> Maybe (FunctionDefinition TypedExpr ConcreteType)
findMethodByName methods methodName =
  find (\f -> (tpName $ functionName f) == methodName) methods
