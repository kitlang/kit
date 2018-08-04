module Kit.Compiler.Typers.TypeVar where

import Control.Monad
import Data.IORef
import Data.List
import Kit.Ast
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

typeVar
  :: CompileContext
  -> Module
  -> VarDefinition TypedExpr ConcreteType
  -> IO (Maybe TypedDecl, Bool)
typeVar ctx mod def@(VarDefinition { varName = name, varNamespace = namespace })
  = do
    tctx              <- newTypeContext []
    binding           <- scopeGet (modScope mod) name
    (typed, complete) <- typeVarDefinition ctx tctx mod def binding
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
  -> Binding
  -> IO (VarDefinition TypedExpr ConcreteType, Bool)
typeVarDefinition ctx tctx mod def binding = do
  -- TODO: type var default expression
  case varDefault def of
    Just x -> do
      resolveConstraint
        ctx
        tctx
        mod
        (TypeEq
          (inferredType x)
          (varType def)
          "Variable and field default values must match the variable's type"
          (varPos def)
        )
    _ -> return ()
  return (def, True)
