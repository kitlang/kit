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
import Kit.Compiler.Utils
import Kit.Error
import Kit.Parser
import Kit.Str

{-
  Type checks a trait implementation.
-}
typeImpl
  :: CompileContext
  -> TypeContext
  -> Module
  -> TraitImplementation TypedExpr ConcreteType
  -> IO TypedDecl
typeImpl ctx tctx' mod def = do
  (traitDef, params) <- case implTrait def of
    TypeTraitConstraint (tp, params) -> do
      traitDef <- getTraitDefinition ctx tp
      params   <- makeGeneric ctx tp (implPos def) params
      return (traitDef, params)
    _ -> throwk $ TypingError
      ("Couldn't find trait " ++ show (implTrait def))
      (implPos def)
  let tctx = (addTypeParams tctx' params) { tctxThis = Just $ implFor def }

  debugLog ctx
    $  "typing trait implemention of "
    ++ s_unpack (showTypePath $ traitName traitDef)
    ++ " for "
    ++ show (implFor def)

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

  return $ DeclImpl $ def { implMethods = methods }

findMethodByName
  :: [FunctionDefinition TypedExpr ConcreteType]
  -> Str
  -> Maybe (FunctionDefinition TypedExpr ConcreteType)
findMethodByName methods methodName =
  find (\f -> (tpName $ functionName f) == methodName) methods
