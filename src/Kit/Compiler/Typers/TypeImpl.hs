module Kit.Compiler.Typers.TypeImpl where

import Control.Monad
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.Base
import Kit.Compiler.Typers.TypeFunction
import Kit.Compiler.Unify
import Kit.Error
import Kit.Parser
import Kit.Str

{-
  Type checks a trait implementation.
-}
typeImpl
  :: CompileContext
  -> Module
  -> TraitImplementation TypedExpr ConcreteType
  -> IO (Maybe TypedDecl, Bool)
typeImpl ctx mod def = do
  tctx'              <- modTypeContext ctx mod
  (traitDef, params) <- case implTrait def of
    TypeTraitConstraint (tp, params) -> do
      traitDef <- getTraitDefinition ctx tp
      params   <- makeGeneric ctx tp (implPos def) params
      return (traitDef, params)
    _ -> throwk $ TypingError
      ("Couldn't find trait " ++ show (implTrait def))
      (implPos def)
  let tctx = (addTypeParams tctx' params) { tctxThis = Just $ implFor def }

  forMWithErrors_ (traitMethods traitDef) $ \method ->
    case findMethodByName (implMethods def) (tpName $ functionName method) of
      Just _  -> return ()
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
    (implMethods def)
    (\method -> do
      (typed, complete) <- typeFunctionDefinition ctx tctx mod method
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

  -- TODO: we're not actually verifying that this matches the trait...
  return (Just $ DeclImpl $ def { implMethods = methods }, True)

findMethodByName
  :: [FunctionDefinition TypedExpr ConcreteType]
  -> Str
  -> Maybe (FunctionDefinition TypedExpr ConcreteType)
findMethodByName methods methodName =
  find (\f -> (tpName $ functionName f) == methodName) methods
