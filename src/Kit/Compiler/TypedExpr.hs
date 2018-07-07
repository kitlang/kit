module Kit.Compiler.TypedExpr where

  import Kit.Ast
  import Kit.Parser.Span

  data TypedExpr = TypedExpr {texpr :: ExprType TypedExpr, tPos :: Span, inferredType :: TypeSpec} deriving (Eq, Show)

  instance ExprWrapper TypedExpr where
    getExpr = texpr
    makeExpr et = TypedExpr {texpr = et, tPos = null_span, inferredType = undefined}
    setExpr ex et = ex {texpr = et}

  makeExprTyped :: ExprType TypedExpr -> TypeSpec -> Span -> TypedExpr
  makeExprTyped et t pos = (makeExpr et) {inferredType = t, tPos = pos}
