module Kit.Compiler.Typers.TypeVar where

import Control.Monad
import Data.IORef
import Data.List
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.Base
import Kit.Compiler.Typers.ConvertExpr
import Kit.Compiler.Typers.TypeExpression
import Kit.Compiler.Unify
import Kit.Error
import Kit.Parser
import Kit.Str

{-
  Type checks a variable declaration.
-}
typeVar
  :: CompileContext
  -> Module
  -> VarDefinition TypedExpr ConcreteType
  -> IO (Maybe TypedDecl, Bool)
typeVar ctx mod def@(VarDefinition { varName = name, varNamespace = namespace })
  = do
    tctx              <- newTypeContext []
    (typed, complete) <- typeVarDefinition ctx tctx mod def
    if complete
      then do
        -- modifyIORef (modTypedContents mod) ((:) $ DeclVar typed)
        return (Just $ DeclVar typed, True)
      else return (Just $ DeclVar typed, False)

typeVarDefinition
  :: CompileContext
  -> TypeContext
  -> Module
  -> VarDefinition TypedExpr ConcreteType
  -> IO (VarDefinition TypedExpr ConcreteType, Bool)
typeVarDefinition ctx tctx mod def = do
  case varDefault def of
    Just x -> do
      typed <- typeExpr ctx tctx mod x
      resolveConstraint
        ctx
        tctx
        (TypeEq
          (varType def)
          (inferredType typed)
          "Variable and field default values must match the variable's type"
          (varPos def)
        )
      return (def { varDefault = Just typed }, True)
    _ -> return (def, True)
