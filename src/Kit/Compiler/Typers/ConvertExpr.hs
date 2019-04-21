module Kit.Compiler.Typers.ConvertExpr (convertExpr) where

import Control.Monad
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Error

{-
  Transforms an Expr into a TypedExpr trivially, without fully typing. Every
  subexpression that needs typing will be typed with either its resolved
  annotation or a new type variable.
-}
convertExpr
  :: CompileContext
  -> TypeContext
  -> Module
  -> [TypePath]
  -> Expr
  -> IO TypedExpr
convertExpr ctx tctx mod params e = do
  let pos'          = pos e
  -- "make type variable"
  let mtv = resolveMaybeType ctx tctx mod params pos' (InferredType pos')
  -- "resolve" - shortcut to recursively call this function on children
  let r             = convertExpr ctx tctx mod params
  -- return a ConcreteType, either the annotated type or a type var if none
  let typeOrTypeVar = resolveMaybeType ctx tctx mod params pos'
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
    Using using e1 -> do
      using' <- mapM
        (convertUsingType (converter r (\_ -> typeOrTypeVar)) pos')
        using
      singleWrapper e1 (Using using')
    Meta    meta e1 -> singleWrapper e1 (Meta meta)
    Literal v    t  -> do
      t' <- resolveMaybeType ctx tctx mod params pos' t
      return $ m (Literal v t') t'
    This                       -> container0 This
    Self                       -> container0 Self
    Identifier Hole            -> container0 (Identifier Hole)
    Identifier (Var id       ) -> container0 (Identifier (Var id))
    Identifier (MacroVar id t) -> do
      t <- case t of
        InferredType _ -> return $ TypeBasicType BasicTypeUnknown
        _              -> resolveType ctx tctx mod t
      return $ m (Identifier (MacroVar id t)) t
    TypeAnnotation e1 t -> do
      t  <- typeOrTypeVar t
      r1 <- r e1
      return $ m (TypeAnnotation r1 t) t
    PreUnop  op e1 -> container1 e1 (PreUnop op)
    PostUnop op e1 -> container1 e1 (PostUnop op)
    Binop op e1 e2 -> container2 e1 e2 (Binop op)
    For   e1 e2 e3 -> do
      r1 <- r e1
      r2 <- r e2
      r3 <- r e3
      return $ m (For r1 r2 r3) TypeVoid
    While e1 e2 d -> do
      r1 <- r e1
      r2 <- r e2
      return $ m (While r1 r2 d) TypeVoid
    If e1 e2 me3 -> do
      t  <- mtv
      r1 <- r e1
      r2 <- r e2
      r3 <- maybeR me3
      return $ m (If r1 r2 r3) t
    Continue   -> return $ m Continue TypeVoid
    Break      -> return $ m Break TypeVoid
    Return me1 -> do
      r1 <- maybeR me1
      return $ m (Return r1) TypeVoid
    Match e1 cases def -> do
      t      <- mtv
      r1     <- r e1
      cases' <- forM cases $ \c -> do
        pattern <- r $ matchPattern c
        body    <- r $ matchBody c
        return $ newMatchCase { matchPattern = pattern, matchBody = body }
      def' <- maybeR def
      return $ m (Match r1 cases' def') t
    InlineCall e1 -> singleWrapper e1 InlineCall
    Field e1 id   -> do
      id <- convertIdentifier typeOrTypeVar id
      container1 e1 (\x -> Field x id)
    FieldWrite e1 id e2 -> do
      id <- convertIdentifier typeOrTypeVar id
      container2 e1 e2 (\x y -> FieldWrite x id y)
    StructInit t fields -> do
      t      <- typeOrTypeVar t
      fields <- forM
        fields
        (\(name, e) -> do
          r1 <- r e
          return (name, r1)
        )
      return $ m (StructInit t fields) t
    UnionInit t (name, e) -> do
      t     <- typeOrTypeVar t
      field <- do
        r1 <- r e
        return (name, r1)
      return $ m (UnionInit t field) t
    -- EnumInit b Str [a]
    ArrayAccess e1 e2      -> container2 e1 e2 ArrayAccess
    ArrayWrite e1 e2  e3   -> container3 e1 e2 e3 ArrayWrite
    Call       e1 imp args -> do
      t    <- mtv
      r1   <- r e1
      imp  <- mapM r imp
      args <- mapM r args
      return $ m (Call r1 imp args) t
    Cast e1 t -> do
      t  <- typeOrTypeVar t
      r1 <- r e1
      return $ m (Cast r1 t) t
    Unsafe e1 -> do
      t  <- makeTypeVar ctx pos'
      r1 <- r e1
      return $ m (Unsafe (r1 { inferredType = t })) t
    BlockComment s     -> return $ m (BlockComment s) TypeVoid
    RangeLiteral e1 e2 -> container2 e1 e2 RangeLiteral
    ArrayLiteral args  -> do
      t    <- mtv
      args <- mapM r args
      return $ m (ArrayLiteral args) (TypeArray t $ length args)
    TupleInit args -> do
      args <- mapM r args
      return $ m (TupleInit args) (TypeTuple (map inferredType args))
    LocalVarDeclaration id t const e1 -> do
      id <- convertIdentifier typeOrTypeVar id
      r1 <- maybeR e1
      t  <- resolveMaybeType ctx tctx mod params pos' t
      return $ m (LocalVarDeclaration id t const r1) t
    -- Defer  e1       -> singleWrapper e1 Defer
    -- Box impl e1 -> do
    --   impl' <- convertTraitImplementation (converter r (\_ -> typeOrTypeVar))
    --                                       modPath
    --                                       impl
    --   let (tp, params) = case implTrait impl' of
    --         TypeTraitConstraint (tp, params) -> (tp, params)
    --   r1 <- r e1
    --   return $ m (Box impl' r1) (TypeBox tp params)
    SizeOf (InferredType _) -> do
      throwk $ BasicError "sizeof keyword requires a type" (Just pos')
    SizeOf t -> do
      t' <- resolveType ctx tctx mod t
      return $ m (SizeOf t') (TypeSize)
    Implicit t -> do
      t' <- resolveType ctx tctx mod t
      return $ m (Implicit t') t'
    Null -> do
      return $ m Null $ TypePtr TypeVoid
    Undefined -> do
      t <- mtv
      return $ m Undefined t
    Empty -> do
      t <- mtv
      return $ m Empty t
    InlineCExpr s t -> do
      t <- resolveMaybeType ctx tctx mod params pos' t
      return $ m (InlineCExpr s t) t
    StaticExpr x -> do
      x <- r x
      t <- mtv
      return $ m (StaticExpr x) t
    Defined id -> do
      id <- convertIdentifier (\_ -> return TypeVoid) id
      return $ m (Defined id) TypeBool
    VarArgListCopy s -> do
      return $ m (VarArgListCopy s) TypeVaList
    _ -> throwk $ InternalError
      ("Can't convert expression: " ++ show (expr e))
      (Just pos')
