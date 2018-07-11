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
  import Kit.Compiler.TypeContext
  import Kit.Compiler.TypedDecl
  import Kit.Compiler.TypedExpr
  import Kit.Compiler.Unify
  import Kit.Compiler.Utils
  import Kit.Error
  import Kit.HashTable
  import Kit.Parser
  import Kit.Str

  typeExpressions :: CompileContext -> IO ()
  typeExpressions ctx = do
    mainModule <- h_get (ctxModules ctx) (ctxMainModule ctx)
    main <- resolveLocal (mod_functions mainModule) "main"
    case main of
      Just main -> do
        typeFunction ctx mainModule main
      Nothing -> throw $ Errs [err ValidationError ("main module " ++ (s_unpack $ showModulePath $ mod_path mainModule) ++ " is missing a main function")]

  basicType = TypeBasicType
  voidType = TypeBasicType BasicTypeVoid

  typeFunction :: CompileContext -> Module -> FunctionDefinition -> IO ()
  typeFunction ctx mod f = do
    case function_body f of
      Just x -> do
        imports <- mapM (\(mp, _) -> getMod ctx mp) (mod_imports mod)
        includes <- mapM (\(mp, _) -> getCMod ctx mp) (mod_includes mod)
        functionScope <- newScope
        tctx <- newTypeContext ((mod_vars mod) : ([mod_vars imp | imp <- imports] ++ [mod_vars inc | inc <- includes]))
        args <- mapM (\arg -> do argType <- resolveMaybeTypeOrFail ctx tctx mod (pos x) (arg_type arg); return (arg_name arg, argType)) (function_args f)
        returnType <- resolveMaybeTypeOrFail ctx tctx mod (pos x) (function_type f)
        typedBody <- typeExpr ctx (tctx {tctxScopes = functionScope : (tctxScopes tctx), tctxReturnType = Just returnType}) mod x
        resolvedReturnType <- knownType ctx tctx returnType
        -- Try to unify with void; if unification doesn't fail, we didn't encounter a return statement, so the function is void.
        let finalReturnType = case unify (resolvedReturnType) (TypeBasicType BasicTypeVoid) of
                                TypeConstraintNotSatisfied -> resolvedReturnType
                                _ -> TypeBasicType BasicTypeVoid
        let typedFunction = TypedFunction {
          typedFunctionName = function_name f,
          typedFunctionReturnType = finalReturnType,
          typedFunctionArgs = args,
          typedFunctionBody = typedBody,
          typedFunctionVariadic = (function_varargs f)
        }
        bindToScope (mod_typed_contents mod) (function_name f) typedFunction
      Nothing -> throw $ Errs [err ValidationError ("main function is missing a function body")]

  {-
    Converts a tree of untyped AST to Typed AST.

    The compile process is roughly:

      untyped AST (Expr)
      -> typed AST (TypedExpr) <- we are generating this
      -> IR (IrExpr)
      -> generated code

    After typing a subexpression, rewrite rules may take effect and trigger typing of the result.
  -}
  typeExpr :: CompileContext -> TypeContext -> Module -> Expr -> IO TypedExpr
  typeExpr ctx tctx mod ex@(Expr {expr = et, pos = pos}) =
    let r = typeExpr ctx tctx mod in
    let resolve constraint = resolveConstraint ctx pos constraint in
    let resolveList constraints = resolveConstraints ctx pos constraints in
    case et of
      (Block children) -> do
        typedChildren <- mapM r children
        return $ makeExprTyped (Block typedChildren) (inferredType $ last typedChildren) pos

      (Meta m e1) -> do
        r1 <- r e1
        return $ makeExprTyped (Meta m r1) (inferredType $ r1) pos

      (Literal l) -> do
        typeVar <- makeTypeVar ctx
        resolveList $ literalConstraints l typeVar
        return $ makeExprTyped (Literal l) typeVar pos

      (This) -> do
        case tctxThis tctx of
          Just t -> return $ makeExprTyped This t pos
          Nothing -> throw $ Errs [errp TypingError ("`this` can only be used in methods") (Just pos)]

      (Self) -> do
        case tctxSelf tctx of
          Just t -> return $ makeExprTyped Self t pos
          Nothing -> throw $ Errs [errp TypingError ("`Self` can only be used in methods") (Just pos)]

      (Lvalue v) -> do
        case v of
          Var vname -> do
            binding <- resolveVar ctx (tctxScopes tctx) mod vname
            case binding of
              Just (VarBinding t) -> return $ makeExprTyped (Lvalue v) t pos
              Just (FunctionBinding f args variadic) -> return $ makeExprTyped (Lvalue v) (TypeFunction f args variadic) pos
              Nothing -> throw $ Errs [errp TypingError ("Unknown identifier: " ++ (s_unpack vname)) (Just pos)]
          MacroVar vname -> throw $ Errs [errp TypingError ("Macro variable $" ++ (s_unpack vname) ++ " can only be used in a rewrite rule") (Just pos)]

      (EnumConstructor s) -> do
        enumConstructor <- resolveEnum ctx tctx mod s
        case enumConstructor of
          Just (t, args) -> if args == []
            then return $ makeExprTyped (EnumConstructor s) (TypeEnum t []) pos
            else return $ makeExprTyped (EnumConstructor s) (TypeEnumConstructor t args) pos
          Nothing -> throw $ Errs [errp TypingError ("Unknown enum constructor: " ++ (s_unpack s)) (Just pos)]

      (TypeAnnotation e1 t) -> do
        r1 <- r e1
        t <- resolveTypeOrFail ctx tctx mod pos t
        resolve $ TypeEq (inferredType r1) t
        return r1

      (PreUnop op e1) -> do
        r1 <- r e1
        tv <- makeTypeVar ctx
        let (t, constraints) = opTypes op tv [inferredType r1]
        resolveList constraints
        return $ makeExprTyped (PreUnop op r1) t pos

      (PostUnop op e1) -> do
        r1 <- r e1
        tv <- makeTypeVar ctx
        let (t, constraints) = opTypes op tv [inferredType r1]
        resolveList constraints
        return $ makeExprTyped (PostUnop op r1) t pos

      (Binop (AssignOp x) e1 e2) -> do
        throw $ Errs [err InternalError "Not yet implemented"]
      (Binop op e1 e2) -> do
        r1 <- r e1
        r2 <- r e2
        tv <- makeTypeVar ctx
        let (t, constraints) = opTypes op tv [inferredType r1, inferredType r2]
        resolveList constraints
        return $ makeExprTyped (Binop op r1 r2) t pos

      (For (Expr {expr = Lvalue v, pos = pos1}) e2 e3) -> do
        r2 <- r e2
        scope <- newScope
        r3 <- typeExpr ctx (tctx {tctxScopes = scope : (tctxScopes tctx), tctxLoopCount = (tctxLoopCount tctx) + 1}) mod e3
        tv <- makeTypeVar ctx
        resolve $ TypeClassMember (TypeIterable tv) (inferredType r2)
        return $ makeExprTyped (For (makeExprTyped (Lvalue v) (TypeLvalue tv) pos1) r2 r3) voidType pos

      (While e1 e2) -> do
        r1 <- r e1
        scope <- newScope
        r2 <- typeExpr ctx (tctx {tctxScopes = scope : (tctxScopes tctx), tctxLoopCount = (tctxLoopCount tctx) + 1}) mod e2
        resolve $ TypeEq (inferredType r1) (basicType BasicTypeBool)
        return $ makeExprTyped (While r1 r2) voidType pos

      (If e1 e2 (Just e3)) -> do
        r1 <- r e1
        scope1 <- newScope
        r2 <- typeExpr ctx (tctx {tctxScopes = scope1 : (tctxScopes tctx)}) mod e2
        scope2 <- newScope
        r3 <- typeExpr ctx (tctx {tctxScopes = scope2 : (tctxScopes tctx)}) mod e3
        tv <- makeTypeVar ctx
        resolveList [TypeEq (inferredType r1) (basicType BasicTypeBool), TypeEq (inferredType r2) (inferredType r3), TypeEq (inferredType r2) (tv)]
        return $ makeExprTyped (If r1 r2 (Just r3)) (tv) pos
      (If e1 e2 Nothing) -> do
        r1 <- r e1
        scope <- newScope
        r2 <- typeExpr ctx (tctx {tctxScopes = scope : (tctxScopes tctx)}) mod e2
        resolve $ TypeEq (inferredType r1) (basicType BasicTypeBool)
        return $ makeExprTyped (If r1 r2 Nothing) voidType pos

      (Continue) -> do
        if (tctxLoopCount tctx > 1)
          then return $ makeExprTyped (Continue) voidType pos
          else throw $ Errs [err TypingError "Can't `continue` outside of a loop"]
      (Break) -> do
        if (tctxLoopCount tctx > 1)
          then return $ makeExprTyped (Break) voidType pos
          else throw $ Errs [err TypingError "Can't `break` outside of a loop"]

      (Return e1) -> do
        case (tctxReturnType tctx, e1) of
          (Just rt, Just e1) -> do
            r1 <- r e1
            resolve $ TypeEq (rt) (inferredType r1)
            return $ makeExprTyped (Return $ Just r1) voidType pos
          (Just rt, Nothing) -> do
            resolve $ TypeEq voidType (rt)
            return $ makeExprTyped (Return Nothing) voidType pos
          (Nothing, _) -> throw $ Errs [err TypingError "Can't `return` outside of a function"]

      (Call e1 args) -> do
        r1 <- r e1
        typedArgs <- mapM r args
        case inferredType r1 of
          TypeFunction _ _ _ -> typeFunctionCall ctx tctx mod r1 typedArgs
          TypeEnumConstructor _ _ -> typeEnumConstructorCall ctx tctx mod r1 typedArgs

      (Throw e1) -> do throw $ Errs [err InternalError "Not yet implemented"]
      (Match e1 cases (e2)) -> throw $ Errs [err InternalError "Not yet implemented"]
      (InlineCall e1) -> throw $ Errs [err InternalError "Not yet implemented"]
      (Field e1 lval) -> throw $ Errs [err InternalError "Not yet implemented"]
      (ArrayAccess e1 e2) -> throw $ Errs [err InternalError "Not yet implemented"]
      (Cast e1 t) -> throw $ Errs [err InternalError "Not yet implemented"]
      (Unsafe e1) -> throw $ Errs [err InternalError "Not yet implemented"]
      (BlockComment s) -> do return $ makeExprTyped (BlockComment s) voidType pos
      (New t args) -> throw $ Errs [err InternalError "Not yet implemented"]
      (Copy e1) -> throw $ Errs [err InternalError "Not yet implemented"]
      (Delete e1) -> throw $ Errs [err InternalError "Not yet implemented"]
      (Move e1) -> throw $ Errs [err InternalError "Not yet implemented"]
      (LexMacro s t) -> throw $ Errs [err InternalError "Not yet implemented"]

      (RangeLiteral e1 e2) -> do
        r1 <- r e1
        r2 <- r e2
        resolveList [TypeClassMember TypeIntegral (inferredType r1), TypeClassMember TypeIntegral (inferredType r2)]
        return $ makeExprTyped (RangeLiteral r1 r2) TypeRange pos

      (VectorLiteral items) -> throw $ Errs [err InternalError "Not yet implemented"]

      (VarDeclaration vardef@(VarDefinition {var_name = Var vname, var_default = Just e1})) -> do
        varType <- resolveMaybeTypeOrFail ctx tctx mod pos (var_type vardef)
        r1 <- r e1
        bindToScope (head $ tctxScopes tctx) vname (VarBinding varType)
        resolve $ TypeEq (varType) (inferredType r1)
        return $ makeExprTyped (VarDeclaration $ ((newVarDefinition :: VarDefinition TypedExpr) {var_name = var_name vardef, var_type = var_type vardef, var_default = Just r1})) voidType pos

      (VarDeclaration vardef@(VarDefinition {var_name = Var vname, var_default = Nothing})) -> do
        varType <- resolveMaybeTypeOrFail ctx tctx mod pos (var_type vardef)
        bindToScope (head $ tctxScopes tctx) vname (VarBinding varType)
        return $ makeExprTyped (VarDeclaration ((newVarDefinition :: VarDefinition TypedExpr) {var_name = var_name vardef, var_type = var_type vardef, var_default = Nothing})) voidType pos

  typeFunctionCall :: CompileContext -> TypeContext -> Module -> TypedExpr -> [TypedExpr] -> IO TypedExpr
  typeFunctionCall ctx tctx mod e@(TypedExpr {inferredType = TypeFunction rt argTypes isVariadic, tPos = pos}) args = do
    -- validate number of arguments
    if isVariadic
      then if length args < length argTypes
           then throw $ Errs [errp TypingError ("Expected " ++ (show $ length argTypes) ++ " or more arguments (called with " ++ (show $ length args) ++ ")") (Just pos)]
           else return ()
      else if length args /= length argTypes
          then throw $ Errs [errp TypingError ("Expected " ++ (show $ length argTypes) ++ " arguments (called with " ++ (show $ length args) ++ ")") (Just pos)]
           else return ()
    -- TODO
    resolveConstraints ctx pos [TypeEq argType (inferredType argValue) | ((_, argType), argValue) <- zip argTypes args]
    return $ makeExprTyped (Call e args) rt pos

  typeEnumConstructorCall :: CompileContext -> TypeContext -> Module -> TypedExpr -> [TypedExpr] -> IO TypedExpr
  typeEnumConstructorCall ctx tctx mod e args = do return e

  literalConstraints :: ValueLiteral -> ConcreteType -> [TypeConstraint]
  literalConstraints (BoolValue _) s = [TypeEq s (basicType $ BasicTypeBool)]
  literalConstraints (IntValue _) s = [TypeClassMember TypeNumeric s]
  literalConstraints (FloatValue _) s = [TypeClassMember TypeFloating s]
  literalConstraints (StringValue _) s = [TypeClassMember TypeString s]

  opTypes :: Operator -> ConcreteType -> [ConcreteType] -> (ConcreteType, [TypeConstraint])
  opTypes op result operands = (result, [TypeClassMember TypeNumeric operand | operand <- (result : operands)])
