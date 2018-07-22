module Kit.Compiler.Typers.TypeType where

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
import Kit.Compiler.Typers.TypeExpression
import Kit.Compiler.Unify
import Kit.Error
import Kit.Parser
import Kit.Str

typeTypeDefinition
  :: CompileContext -> Module -> TypeDefinition Expr (Maybe TypeSpec) -> IO ()
typeTypeDefinition ctx mod def@(TypeDefinition { typeName = name }) = do
  -- TODO: handle params here
  tctx      <- newTypeContext []
  converted <- convertTypeDefinition (typeExpr ctx tctx mod)
                                     (resolveMaybeType ctx tctx mod null_span) -- FIXME: position
                                     def
  bindToScope (modTypedContents mod) name (TypedTypeDecl converted)
