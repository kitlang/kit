module Kit.Compiler.Monomorphize where

import Kit.Ast
import Kit.Str

substituteTypeSpec :: TypeSpec -> [(TypeSpec, TypeSpec)] -> TypeSpec
substituteTypeSpec t ((pattern, replacement) : substitutions) =
  if t == pattern then replacement else substituteTypeSpec t substitutions
substituteTypeSpec t [] = t

monomorphizeExpr :: [(TypeSpec, TypeSpec)] -> Expr -> Expr
monomorphizeExpr subs = exprMap (monomorphizeExpr1 subs)

monomorphizeExpr1 :: [(TypeSpec, TypeSpec)] -> Expr -> Expr
monomorphizeExpr1 substitutions e@(Expr { expr = (TypeAnnotation e1 t) }) =
  e { expr = TypeAnnotation e1 (substituteTypeSpec t substitutions) }
monomorphizeExpr1 substitutions e@(Expr { expr = (Cast e1 t) }) =
  e { expr = Cast e1 (substituteTypeSpec t substitutions) }
monomorphizeExpr1 substitutions e = e

monomorphizeType :: [(TypeSpec, TypeSpec)] -> TypeDefinition -> TypeDefinition
monomorphizeType substitutions t = t -- TODO
