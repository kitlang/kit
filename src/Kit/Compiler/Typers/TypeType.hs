module Kit.Compiler.Typers.TypeType where

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
import Kit.Compiler.Typers.TypeExpression
import Kit.Compiler.Typers.TypeFunction
import Kit.Compiler.Typers.TypeVar
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.Parser
import Kit.Str

typeTypeDefinition
  :: CompileContext -> Module -> TypeDefinition Expr (Maybe TypeSpec) -> IO ()
typeTypeDefinition ctx mod def@(TypeDefinition { typeName = name }) = do
  debugLog ctx $ "typing " ++ s_unpack name ++ " in " ++ show mod
  -- TODO: handle params here
  tctx     <- newTypeContext []
  subScope <- getSubScope (modScope mod) [name]
  let exprConverter = typeExpr ctx tctx mod
  let typeConverter = resolveMaybeType ctx tctx mod (typePos def)
  staticFields <- forM
    (typeStaticFields def)
    (\field -> do
      binding <- scopeGet subScope (varName field)
      typeVarDefinition ctx tctx mod field binding
    )
  staticMethods <- forM
    (typeStaticMethods def)
    (\method -> do
      binding <- scopeGet subScope (functionName method)
      typeFunctionDefinition ctx tctx mod method binding
    )
  converted <- convertTypeDefinition exprConverter typeConverter def
  modifyIORef
    (modTypedContents mod)
    ((:) $ DeclType $ converted { typeStaticFields  = staticFields
                                , typeStaticMethods = staticMethods
                                }
    )
