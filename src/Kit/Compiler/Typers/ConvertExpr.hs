module Kit.Compiler.Typers.ConvertExpr where

import Control.Applicative
import Control.Monad
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TermRewrite
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.Base
import Kit.Compiler.Unify
import Kit.Error
import Kit.HashTable
import Kit.Parser
import Kit.Str

{-
  Transforms an Expr into a TypedExpr trivially, without fully typing. Every
  subexpression that needs typing will be typed with either its resolved
  annotation or a new type variable.
-}
convertExpr :: CompileContext -> TypeContext -> Module -> Expr -> IO TypedExpr
convertExpr ctx tctx mod e = do
  let pos'          = pos e
  -- "make type variable"
  let mtv           = makeTypeVar ctx pos'
  -- "resolve" - shortcut to recursively call this function on children
  let r             = convertExpr ctx tctx mod
  -- return a ConcreteType, either the annotated type or a type var if none
  let typeOrTypeVar = resolveMaybeType ctx tctx mod pos'
  -- resolve, but for Maybe Expr
  let maybeR x =
        (case x of
          Just x -> do
            rx <- r x
            return $ Just rx
          Nothing -> return Nothing
        ) :: IO (Maybe TypedExpr)
  -- shortcut to make a TypedExpr from ExprType and ConcreteType
  let m a b = makeExprTyped a b pos'
  -- shortcut for AST nodes that wrap an inner node and have the same type
  let singleWrapper e1 f = do
        r1 <- r e1
        return $ m (f r1) (inferredType r1)
  -- shortcuts for AST nodes with N children, typed as a new type var
  let container0 f = do
        t <- mtv
        return $ m f t
  let container1 e1 f = do
        t  <- mtv
        r1 <- r e1
        return $ m (f r1) t
  let container2 e1 e2 f = do
        t  <- mtv
        r1 <- r e1
        r2 <- r e2
        return $ m (f r1 r2) t
  let container3 e1 e2 e3 f = do
        t  <- mtv
        r1 <- r e1
        r2 <- r e2
        r3 <- r e3
        return $ m (f r1 r2 r3) t

  case expr e of
    Block x -> do
      t        <- mtv
      children <- mapM r x
      return $ makeExprTyped (Block children) t pos'
    Using using e1 -> singleWrapper e1 (Using $ map convertUsingType using)
    Meta  meta  e1 -> singleWrapper e1 (Meta meta)
    Literal l      -> container0 (Literal l)
    This           -> container0 This
    Self           -> container0 Self
    Identifier (Var id       ) namespace -> container0 (Identifier (Var id) namespace)
    Identifier (MacroVar id t) namespace -> do
      t <- case t of
        Just x  -> resolveType ctx tctx mod x
        Nothing -> return $ TypeBasicType BasicTypeUnknown
      return $ m (Identifier (MacroVar id t) namespace) t
    TypeAnnotation e1 t -> do
      t  <- typeOrTypeVar t
      r1 <- r e1
      return $ m (TypeAnnotation r1 t) t
    PreUnop  op e1 -> container1 e1 (PreUnop op)
    PostUnop op e1 -> container1 e1 (PostUnop op)
    Binop op e1 e2 -> container2 e1 e2 (Binop op)
    For   e1 e2 e3 -> container3 e1 e2 e3 (For)
    While e1 e2    -> do
      r1 <- r e1
      r2 <- r e2
      return $ m (While r1 r2) voidType
    If e1 e2 me3 -> do
      t  <- mtv
      r1 <- r e1
      r2 <- r e2
      r3 <- maybeR me3
      return $ m (If r1 r2 r3) t
    Continue   -> return $ m Continue voidType
    Break      -> return $ m Break voidType
    Return me1 -> do
      r1 <- maybeR me1
      return $ m (Return r1) voidType
    Throw e1 -> do
      r1 <- r e1
      return $ m (Throw r1) voidType
    -- Match a [MatchCase a] (Maybe a) -> do
    InlineCall e1 -> singleWrapper e1 InlineCall
    Field e1 id   -> do
      id <- convertIdentifier typeOrTypeVar id
      container1 e1 (\x -> Field x id)
    StructInit t fields -> do
      t      <- typeOrTypeVar t
      fields <- forM
        fields
        (\(name, e) -> do
          r1 <- r e
          return (name, r1)
        )
      return $ m (StructInit t fields) t
    -- EnumInit b Str [a]
    ArrayAccess e1 e2   -> container2 e1 e2 ArrayAccess
    Call        e1 args -> do
      t    <- mtv
      r1   <- r e1
      args <- mapM r args
      return $ m (Call r1 args) t
    Cast e1 t -> do
      t  <- typeOrTypeVar t
      r1 <- r e1
      return $ m (Cast r1 t) t
    Unsafe       e1    -> container1 e1 Unsafe
    BlockComment s     -> return $ m (BlockComment s) voidType
    RangeLiteral e1 e2 -> container2 e1 e2 RangeLiteral
    VectorLiteral args -> do
      t    <- mtv
      args <- mapM r args
      return $ m (VectorLiteral args) t
    VarDeclaration id t e1 -> do
      id <- convertIdentifier typeOrTypeVar id
      t  <- resolveMaybeType ctx tctx mod pos' t
      r1 <- maybeR e1
      return $ m (VarDeclaration id t r1) t
    Defer e1 -> singleWrapper e1 Defer
