module Kit.Compiler.Typers.TypeVar where

import Control.Monad
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedStmt
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.TypeExpression
import Kit.Compiler.Unify
import Kit.Error

{-
  Type checks a variable declaration.
-}
typeVar
  :: CompileContext
  -> TypeContext
  -> Module
  -> VarDefinition TypedExpr ConcreteType
  -> IO TypedStmt
typeVar ctx tctx mod def@(VarDefinition { varName = name }) = do
  typed <- typeVarDefinition ctx tctx mod def
  return $ ps (varPos def) $ VarDeclaration typed

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
    Nothing -> do
      when (varIsConst def) $ throwk $ TypingError
        ("const must have an initial value")
        (varPos def)
      return def
