module Kit.Compiler.Typers.TypeTrait where

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
import Kit.Log
import Kit.Parser
import Kit.Str

{-
  Type checks a trait specification.
-}
typeTrait
  :: CompileContext
  -> Module
  -> TraitDefinition TypedExpr ConcreteType
  -> IO (Maybe TypedDecl, Bool)
typeTrait ctx mod def = do
  debugLog ctx $ "typing trait " ++ s_unpack (showTypePath $ traitName def)
  tctx <- modTypeContext ctx mod
  typeTraitDefinition ctx tctx mod (TypeTraitConstraint (traitName def, [])) def

{-
  Type checks a specific monomorph of a generic trait, with a known set of
  parameters. By this point the final types of the parameters must be known.
-}
typeTraitMonomorph
  :: CompileContext
  -> Module
  -> TraitDefinition TypedExpr ConcreteType
  -> [ConcreteType]
  -> IO (Maybe TypedDecl, Bool)
typeTraitMonomorph ctx mod def params = do
  debugLog ctx
    $  "generating trait monomorph for "
    ++ s_unpack (showTypePath $ traitName def)
    ++ " with params "
    ++ show params
  -- let selfType = TypeInstance (modPath mod, typeName def) params
  tctx' <- modTypeContext ctx mod
  let tctx =
        (addTypeParams
          tctx'
          [ (traitSubPath def $ paramName param, ct)
          | (param, ct) <- zip (traitParams def) params
          ]
        )
  monomorph <- followTrait ctx tctx def
  typeTraitDefinition ctx
                      tctx
                      mod
                      (TypeTraitConstraint (traitName def, params))
                      monomorph

{-
  Type checks a trait specification.
-}
typeTraitDefinition
  :: CompileContext
  -> TypeContext
  -> Module
  -> ConcreteType
  -> TraitDefinition TypedExpr ConcreteType
  -> IO (Maybe TypedDecl, Bool)
typeTraitDefinition ctx tctx' mod selfType def = do
  let tctx = tctx' { tctxSelf = Just selfType, tctxThis = Just selfType }
  methods <- forM
    (traitMethods def)
    (\method -> do
      (typed, complete) <- typeFunctionDefinition ctx tctx mod method
      return typed
    )
  return (Just $ DeclTrait $ def { traitMethods = methods }, True)
