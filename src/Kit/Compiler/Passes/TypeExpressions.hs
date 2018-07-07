module Kit.Compiler.Passes.TypeExpressions where

  import Control.Exception
  import Control.Monad
  import Data.IORef
  import Data.List
  import System.FilePath
  import Kit.Ast
  import Kit.Compiler.Context
  import Kit.Compiler.Module
  import Kit.Compiler.Scope
  import Kit.Compiler.TypedExpr
  import Kit.Compiler.Utils
  import Kit.Error
  import Kit.HashTable
  import Kit.Parser
  import Kit.Str

  data TypeContext = TypeContext {
    tctxScopes :: [Scope Binding],
    tctxReturnVal :: Maybe TypeSpec,
    tctxThis :: Maybe TypeSpec,
    tctxSelf :: Maybe TypeSpec
  }

  newTypeContext :: [Scope Binding] -> IO TypeContext
  newTypeContext scopes = do
    scope <- newScope
    lastTypeVar <- newIORef 0
    return $ TypeContext {tctxScopes = scope : scopes, tctxReturnVal = Nothing, tctxThis = Nothing, tctxSelf = Nothing}

  typeExpressions :: CompileContext -> IO ()
  typeExpressions ctx = do
    mainModule <- h_get (ctxModules ctx) (ctxMainModule ctx)
    main <- resolveLocal (mod_vars mainModule) "main"
    putStrLn $ show (main)
    return ()

  {-
    Converts a tree of untyped AST to Typed AST.

    The compile process is roughly:

      untyped AST (Expr)
      -> typed AST (TypedExpr) <- we are here
      -> IR (IrExpr)
      -> generated code

    After typing, rewrite rules may take effect.
  -}
  typeExpr :: CompileContext -> TypeContext -> Module -> Expr -> IO (TypedExpr, [TypeConstraint])
  typeExpr ctx tctx mod ex@(Expr {expr = et, pos = pos}) =
    let r = typeExpr ctx tctx mod in
    case et of
    (Block children) -> do
      results <- mapM r children
      let (typedChildren, constraints) = (map fst results, foldr (++) [] (map snd results))
      return (makeExprTyped (Block typedChildren) (inferredType $ last typedChildren) pos, constraints)
    (Meta m e1) -> do
      result <- r e1
      return $ (makeExprTyped (Meta m (fst result)) (inferredType $ fst result) pos, snd result)
    (Literal l) -> do
      typeVar <- makeTypeVar ctx
      return (makeExprTyped (Literal l) typeVar pos, literalConstraints l typeVar)
    (This) -> do
      case tctxThis tctx of
        Just t -> return (makeExprTyped This t pos, [])
        Nothing -> throw $ Errs [errp TypingError ("`this` can only be used in methods") (Just pos)]
    (Self) -> do
      case tctxSelf tctx of
        Just t -> return (makeExprTyped Self t pos, [])
        Nothing -> throw $ Errs [errp TypingError ("`Self` can only be used in methods") (Just pos)]
    (Lvalue v) -> do
      case v of
        Var vname -> do
          binding <- resolveVar ctx (tctxScopes tctx) mod vname
          case binding of
            Just (VarBinding t) -> return (makeExprTyped (Lvalue v) t pos, [])
            Just (FunctionBinding f args variadic) -> return (makeExprTyped (Lvalue v) (TypeFunctionSpec f args variadic) pos, [])
            Nothing -> throw $ Errs [errp TypingError ("Unknown identifier: " ++ (s_unpack vname)) (Just pos)]
        MacroVar vname -> throw $ Errs [errp TypingError ("Macro variable $" ++ (s_unpack vname) ++ " can only be used in a rewrite rule") (Just pos)]
    -- (TypeConstructor s) -> (TypeConstructor s)
    -- (TypeAnnotation e1 t) -> (TypeAnnotation ((exprMap f) e1) t)
    -- (PreUnop op e1) -> (PreUnop op ((exprMap f) e1))
    -- (PostUnop op e1) -> (PostUnop op ((exprMap f) e1))
    -- (Binop op e1 e2) -> (Binop op ((exprMap f) e1) ((exprMap f) e2))
    -- (For e1 e2 e3) -> (For ((exprMap f) e1) ((exprMap f) e2) ((exprMap f) e3))
    -- (While e1 e2) -> (While ((exprMap f) e1) ((exprMap f) e2))
    -- (If e1 e2 e3) -> (If ((exprMap f) e1) ((exprMap f) e2) (mapMaybeExpr (exprMap f) e3))
    -- (Continue) -> (Continue)
    -- (Break) -> (Break)
    -- (Return e1) -> (Return (mapMaybeExpr (exprMap f) e1))
    -- (Throw e1) -> (Throw ((exprMap f) e1))
    -- (Match e1 cases (e2)) -> (Match ((exprMap f) e1) [MatchCase {match_pattern = (exprMap f) $ match_pattern c, match_body = (exprMap f) $ match_body c} | c <- cases] (mapMaybeExpr (exprMap f) e2))
    -- (InlineCall e1) -> (InlineCall ((exprMap f) e1))
    -- (Field e1 lval) -> (Field ((exprMap f) e1) lval)
    -- (ArrayAccess e1 e2) -> (ArrayAccess ((exprMap f) e1) ((exprMap f) e2))
    -- (Call e1 args) -> (Call ((exprMap f) e1) (map (exprMap f) args))
    -- (Cast e1 t) -> (Cast ((exprMap f) e1) t)
    -- (Unsafe e1) -> (Unsafe ((exprMap f) e1))
    -- (BlockComment s) -> (BlockComment s)
    -- (New t args) -> (New t (map (exprMap f) args))
    -- (Copy e1) -> (Copy ((exprMap f) e1))
    -- (Delete e1) -> (Delete ((exprMap f) e1))
    -- (Move e1) -> (Move ((exprMap f) e1))
    -- (LexMacro s t) -> (LexMacro s t)
    -- (RangeLiteral e1 e2) -> (RangeLiteral ((exprMap f) e1) ((exprMap f) e2))
    -- (VectorLiteral items) -> (VectorLiteral (map (exprMap f) items))
    -- (VarDeclaration vardef) -> (VarDeclaration (vardef {var_default = mapMaybeExpr (exprMap f) $ var_default vardef}))

  literalConstraints :: ValueLiteral -> TypeSpec -> [TypeConstraint]
  literalConstraints (BoolValue _) s = [TypeEq s (ConcreteType $ TypeBasicType $ BasicTypeBool)]
  literalConstraints (IntValue _) s = [TypeNumeric s]
  literalConstraints (FloatValue _) s = [TypeFloating s]
  literalConstraints (StringValue _) s = [TypeString s]
