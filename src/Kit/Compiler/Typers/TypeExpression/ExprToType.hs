module Kit.Compiler.Typers.TypeExpression.ExprToType(exprToType) where

import Control.Monad
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr

exprToType
  :: CompileContext
  -> TypeContext
  -> Module
  -> Span
  -> TypedExpr
  -> IO (Maybe ConcreteType)
exprToType ctx tctx mod pos ex = do
  let r x = exprToType ctx tctx mod pos x
  x <- case (tExpr ex, inferredType ex) of
    (_, TypeTypeOf tp params) -> return $ Just $ TypeInstance tp params
    (Identifier (Var s), _) -> do
      t <- resolveType ctx tctx mod $ TypeSpec s [] pos
      return $ Just t
    (TupleInit x, _) -> do
      x <- forM x $ r
      let result = sequence x
      case result of
        Just x -> do
          return $ Just $ TypeTuple x
        _ -> return Nothing
    (ArrayAccess a b, _) -> do
      b <- r b
      case b of
        Just b -> do
          a <- r a
          case a of
            Just t@(TypeInstance tp params) -> do
              params <- forM (params ++ [b]) $ makeGenericConcrete ctx pos
              return $ Just $ TypeInstance tp $ params
            _ -> return Nothing
        _ -> return Nothing
    (PreUnop Deref x, _) -> do
      x <- r x
      case x of
        Just x -> return $ Just $ TypePtr x
        _      -> return Nothing
    _ -> return Nothing
  case x of
    Just t -> genericTctx ctx tctx pos t >> return ()
    _      -> return ()
  return x
