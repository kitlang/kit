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
  tctx' <- modTypeContext ctx mod
  let tctx = tctx'
        { tctxThis       = Just $ implFor def
        -- , tctxTypeParams = [ (paramName param, ()) | param <- traitParams def ]
        }
  methods <- forM
    (implMethods def)
    (\method -> do
      (typed, complete) <- typeFunctionDefinition ctx tctx mod method
      return typed
    )

  -- TODO: we're not actually verifying that this matches the trait...
  return (Just $ DeclImpl $ def { implMethods = methods }, True)
