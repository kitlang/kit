module Kit.Compiler.Typers.TypeVar where

import Control.Monad
import Data.IORef
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.Base
import Kit.Compiler.Typers.TypeExpression
import Kit.Compiler.Unify
import Kit.Error
import Kit.Parser
import Kit.Str

typeVar
  :: CompileContext -> Module -> VarDefinition Expr (Maybe TypeSpec) -> IO ()
typeVar ctx mod def@(VarDefinition { varName = name, varNamespace = namespace })
  = do
    tctx    <- newTypeContext []
    binding <- scopeGet (modScope mod) name
    typed   <- typeVarDefinition ctx tctx mod def binding
    modifyIORef (modTypedContents mod) ((:) $ DeclVar typed)

typeVarDefinition
  :: CompileContext
  -> TypeContext
  -> Module
  -> VarDefinition Expr (Maybe TypeSpec)
  -> Binding
  -> IO (VarDefinition TypedExpr ConcreteType)
typeVarDefinition ctx tctx mod def binding = do
  typed' <- convertVarDefinition (typeExpr ctx tctx mod)
                                 (resolveMaybeType ctx tctx mod (varPos def))
                                 def
  let typed = typed' { varType = bindingConcrete binding }
  case varDefault typed of
    Just x -> do
      resolveConstraint
        ctx
        tctx
        mod
        (TypeEq
          (inferredType x)
          (varType typed)
          "Variable and field default values must match the variable's type"
          (varPos def)
        )
    _ -> return ()
  return typed
