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
  -> TypeContext
  -> Module
  -> VarDefinition TypedExpr ConcreteType
  -> IO TypedDecl
typeVar ctx tctx mod def@(VarDefinition { varName = name }) = do
  typed <- typeVarDefinition ctx tctx mod def
  return $ DeclVar typed

typeVarDefinition
  :: CompileContext
  -> TypeContext
  -> Module
  -> VarDefinition TypedExpr ConcreteType
  -> IO (VarDefinition TypedExpr ConcreteType)
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
      return $ def { varDefault = Just typed }
    _ -> return def
