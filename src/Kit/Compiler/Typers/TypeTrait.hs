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
  tctx <- modTypeContext ctx mod
  typeTraitDefinition ctx tctx mod def

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
    ++ s_unpack (traitName def)
    ++ " in "
    ++ show mod
  -- let selfType = TypeInstance (modPath mod, typeName def) params
  tctx' <- modTypeContext ctx mod
  let tctx =
        (addTypeParams
          tctx'
          [ (traitSubPath (modPath mod) def $ paramName param, ct)
          | (param, ct) <- zip (traitParams def) params
          ]
        )
        -- { tctxSelf = Just selfType
        -- }
  -- monomorph <- followType ctx tctx (modPath mod) def
  typeTraitDefinition ctx tctx mod def

{-
  Type checks a trait specification.
-}
typeTraitDefinition
  :: CompileContext
  -> TypeContext
  -> Module
  -> TraitDefinition TypedExpr ConcreteType
  -> IO (Maybe TypedDecl, Bool)
typeTraitDefinition ctx tctx mod def = do
  methods <- forM
    (traitMethods def)
    (\method -> do
      (typed, complete) <- typeFunctionDefinition ctx tctx mod method
      return typed
    )
  return (Just $ DeclTrait $ def { traitMethods = methods }, True)
