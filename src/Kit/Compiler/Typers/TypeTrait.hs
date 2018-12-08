module Kit.Compiler.Typers.TypeTrait where

import Control.Monad
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedStmt
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.TypeFunction
import Kit.Compiler.Utils
import Kit.Str

{-
  Type checks a trait specification.
-}
typeTrait
  :: CompileContext
  -> TypeContext
  -> Module
  -> TraitDefinition TypedExpr ConcreteType
  -> IO TypedStmt
typeTrait ctx tctx mod def = do
  debugLog ctx
    $  "typing trait "
    ++ s_unpack (showTypePath $ traitName def)
    ++ (case traitMonomorph def of
         [] -> ""
         x  -> " monomorph " ++ show x
       )
  typeTraitDefinition ctx tctx mod (TypeTraitConstraint (traitName def, [])) def

{-
  Type checks a trait specification.
-}
typeTraitDefinition
  :: CompileContext
  -> TypeContext
  -> Module
  -> ConcreteType
  -> TraitDefinition TypedExpr ConcreteType
  -> IO TypedStmt
typeTraitDefinition ctx tctx' mod selfType def = do
  let tctx = tctx' { tctxSelf = Just selfType, tctxThis = Just selfType }
  methods <- forM
    (traitMethods def)
    (\method -> do
      typed <- typeFunctionDefinition ctx tctx mod method
      return typed
    )
  return $ ps (traitPos def) $ TraitDeclaration $ def { traitMethods = methods }
