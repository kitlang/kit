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
  mods <- ctxSourceModules ctx
  forM_ mods (typeModuleExpressions ctx)

typeModuleExpressions :: CompileContext -> Module -> IO ()
typeModuleExpressions ctx mod = do
  bindings <- bindingList (modFunctions mod)
  forM_ bindings (typeFunction ctx mod)

basicType = TypeBasicType
voidType = TypeBasicType BasicTypeVoid

typeFunction
  :: CompileContext
  -> Module
  -> FunctionDefinition Expr (Maybe TypeSpec)
  -> IO ()
typeFunction ctx mod f = do
  debugLog ctx
    $  "typing function "
    ++ s_unpack (functionName f)
    ++ " in "
    ++ show mod
  case functionBody f of
    Just x -> do
      let isMain =
            (functionName f == "main") && (modPath mod == ctxMainModule ctx)
      imports       <- mapM (\(mp, _) -> getMod ctx mp) (modImports mod)
      includes      <- mapM (\(mp, _) -> getCMod ctx mp) (modIncludes mod)
      functionScope <- newScope
      tctx          <- newTypeContext
        ( (modVars mod)
        : (  [ modVars imp | imp <- imports ]
          ++ [ modVars inc | inc <- includes ]
          )
        )
      args <- mapM
        (\arg -> do
          argType <- resolveMaybeType ctx tctx mod (pos x) (argType arg)
          return $ newArgSpec { argName = argName arg, argType = argType }
        )
        (functionArgs f)
      forM_
        args
        (\arg -> bindToScope functionScope
                             (argName arg)
                             (newBinding (VarBinding $ argType arg) Nothing)
        )
      returnType <- resolveMaybeType ctx tctx mod (pos x) (functionType f)
      typedBody  <- typeExpr
        ctx
        (tctx { tctxScopes     = functionScope : (tctxScopes tctx)
              , tctxReturnType = Just returnType
              }
        )
        mod
        x
      resolvedReturnType <- knownType ctx tctx mod returnType
      -- Try to unify with void; if unification doesn't fail, we didn't encounter a return statement, so the function is void.
      unification        <- unify ctx
                                  tctx
                                  mod
                                  (resolvedReturnType)
                                  (TypeBasicType BasicTypeVoid)
      let finalReturnType = case unification of
            TypeConstraintNotSatisfied -> resolvedReturnType
            _                          -> TypeBasicType BasicTypeVoid
      let
        typedFunction = ((convertFunctionDefinition f) :: TypedFunction)
          { functionName         = functionName f
          , functionType         = finalReturnType
          , functionArgs         = args
          , functionBody         = Just typedBody
          , functionVarargs      = (functionVarargs f)
          , functionNameMangling = (if isMain
                                     then Nothing
                                     else functionNameMangling f
                                   )
          }
      bindToScope (modTypedContents mod)
                  (functionName f)
                  (TypedFunction typedFunction)
    _ -> return ()

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
typeExpr ctx tctx mod ex@(Expr { expr = et, pos = pos }) = do
  let r = typeExpr ctx tctx mod
  let resolve constraint = resolveConstraint ctx tctx mod constraint
  result <- case et of
    (Block children) -> do
      typedChildren <- mapM r children
      return $ makeExprTyped
        (Block typedChildren)
        (if children == [] then voidType else inferredType $ last typedChildren)
        pos

    (Meta m e1) -> do
      r1 <- r e1
      return $ makeExprTyped (Meta m r1) (inferredType $ r1) pos

    (Literal l) -> do
      typeVar <- makeTypeVar ctx pos
      mapM_ resolve $ literalConstraints l typeVar pos
      return $ makeExprTyped (Literal l) typeVar pos

    (This) -> do
      case tctxThis tctx of
        Just t -> return $ makeExprTyped This t pos
        Nothing ->
          throwk $ TypingError ("`this` can only be used in methods") pos

    (Self) -> do
      case tctxSelf tctx of
        Just t -> return $ makeExprTyped Self t pos
        Nothing ->
          throwk $ TypingError ("`Self` can only be used in methods") pos

    (Identifier v _) -> do
      case v of
        Var vname -> do
          binding <- resolveVar ctx (tctxScopes tctx) mod vname
          case binding of
            Just (Binding { bindingType = VarBinding t, bindingNameMangling = mangle })
              -> return $ makeExprTyped (Identifier v mangle) t pos
            Just (Binding { bindingType = FunctionBinding f args variadic, bindingNameMangling = mangle })
              -> return $ makeExprTyped (Identifier v mangle)
                                        (TypeFunction f args variadic)
                                        pos
            Just (Binding { bindingType = EnumConstructor t args }) ->
              -- TODO: handle type params
              return $ makeExprTyped (Identifier v Nothing) t' pos
             where
              t' = if args == []
                then (TypeEnum t [])
                else (TypeEnumConstructor t args)
            Nothing -> throwk
              $ TypingError ("Unknown identifier: " ++ (s_unpack vname)) pos
        MacroVar vname _ -> throwk $ TypingError
          (  "Macro variable $"
          ++ (s_unpack vname)
          ++ " can only be used in a rewrite rule"
          )
          pos

    (TypeAnnotation e1 t) -> do
      r1 <- r e1
      t  <- resolveMaybeType ctx tctx mod pos t
      resolve $ TypeEq (inferredType r1) t "Annotated expressions must match their type annotation" (tPos r1)
      return r1

    (PreUnop op e1) -> do
      r1 <- r e1
      tv <- makeTypeVar ctx pos
      case unopTypes op (inferredType r1) tv pos of
        Just constraints -> mapM_ resolve constraints
        Nothing          -> return () -- TODO
      return $ makeExprTyped (PreUnop op r1) tv pos

    (PostUnop op e1) -> do
      r1 <- r e1
      tv <- makeTypeVar ctx pos
      case unopTypes op (inferredType r1) tv pos of
        Just constraints -> mapM_ resolve constraints
        Nothing          -> return () -- TODO
      return $ makeExprTyped (PostUnop op r1) tv pos

    (Binop Assign e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      resolve $ TypeEq (inferredType r1) (inferredType r2) "Both sides of an assignment must unify" pos
      return $ makeExprTyped (Binop Assign r1 r2) (inferredType r1) pos
    (Binop (AssignOp op) e1 e2) | (op == And) || (op == Or) -> do
      r1 <- r e1
      r2 <- r e2
      resolve $ TypeEq (inferredType r1) (inferredType r2) "Both sides of an assignment must unify" pos
      return $ makeExprTyped
        (Binop Assign r1 (makeExprTyped (Binop op r1 r2) (inferredType r1) pos))
        (inferredType r1)
        pos
    (Binop (AssignOp x) e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      -- FIXME: this isn't right
      resolve $ TypeEq (inferredType r1) (inferredType r2) "FIXME: this isn't right" pos
      return $ makeExprTyped (Binop (AssignOp x) r1 r2) (inferredType r1) pos
    (Binop op e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      tv <- makeTypeVar ctx pos
      case binopTypes op (inferredType r1) (inferredType r2) tv pos of
        Just constraints -> mapM_ resolve constraints
        Nothing          -> return () -- TODO
      return $ makeExprTyped (Binop op r1 r2) tv pos

    (For (Expr { expr = Identifier v _, pos = pos1 }) e2 e3) -> do
      r2    <- r e2
      scope <- newScope
      r3    <- typeExpr
        ctx
        (tctx { tctxScopes    = scope : (tctxScopes tctx)
              , tctxLoopCount = (tctxLoopCount tctx) + 1
              }
        )
        mod
        e3
      tv <- makeTypeVar ctx pos
      resolve $ TypeEq (typeClassIterable tv) (inferredType r2) "For statements must iterate over an Iterable type" (tPos r2)
      return $ makeExprTyped
        (For (makeExprTyped (Identifier v Nothing) (TypeIdentifier tv) pos1)
             r2
             r3
        )
        voidType
        pos

    (While e1 e2) -> do
      r1    <- r e1
      scope <- newScope
      r2    <- typeExpr
        ctx
        (tctx { tctxScopes    = scope : (tctxScopes tctx)
              , tctxLoopCount = (tctxLoopCount tctx) + 1
              }
        )
        mod
        e2
      resolve $ TypeEq (inferredType r1) (basicType BasicTypeBool) "A while condition must be a Bool" (tPos r1)
      return $ makeExprTyped (While r1 r2) voidType pos

    (If e1 e2 (Just e3)) -> do
      r1     <- r e1
      scope1 <- newScope
      r2     <- typeExpr ctx
                         (tctx { tctxScopes = scope1 : (tctxScopes tctx) })
                         mod
                         e2
      scope2 <- newScope
      r3     <- typeExpr ctx
                         (tctx { tctxScopes = scope2 : (tctxScopes tctx) })
                         mod
                         e3
      tv <- makeTypeVar ctx pos
      resolve $ TypeEq (inferredType r1) (basicType BasicTypeBool) "An if condition must be a Bool" (tPos r1)
      resolve $ TypeEq (inferredType r2) (inferredType r3) "In an if expression with an else clause, both clauses must have the same type" pos
      resolve $ TypeEq (inferredType r2) (tv) "The type of an if expression must match its clauses" (tPos r2)
      return $ makeExprTyped (If r1 r2 (Just r3)) (tv) pos
    (If e1 e2 Nothing) -> do
      r1    <- r e1
      scope <- newScope
      r2    <- typeExpr ctx
                        (tctx { tctxScopes = scope : (tctxScopes tctx) })
                        mod
                        e2
      resolve $ TypeEq (inferredType r1) (basicType BasicTypeBool) "An if condition must be a Bool" (tPos r1)
      return $ makeExprTyped (If r1 r2 Nothing) voidType pos

    (Continue) -> do
      if (tctxLoopCount tctx > 0)
        then return $ makeExprTyped (Continue) voidType pos
        else throwk $ TypingError "Can't `continue` outside of a loop" pos
    (Break) -> do
      if (tctxLoopCount tctx > 0)
        then return $ makeExprTyped (Break) voidType pos
        else throwk $ TypingError "Can't `break` outside of a loop" pos

    (Return e1) -> do
      case (tctxReturnType tctx, e1) of
        (Just rt, Just e1) -> do
          r1 <- r e1
          resolve $ TypeEq (rt)
                           (inferredType r1)
                           "Return type should match function return type"
                           (tPos r1)
          return $ makeExprTyped (Return $ Just r1) voidType pos
        (Just rt, Nothing) -> do
          resolve $ TypeEq voidType
                           (rt)
                           "Empty return is only allowed in Void functions"
                           pos
          return $ makeExprTyped (Return Nothing) voidType pos
        (Nothing, _) ->
          throwk $ TypingError "Can't `return` outside of a function" pos

    (Call e1 args) -> do
      r1        <- r e1
      typedArgs <- mapM r args
      case inferredType r1 of
        TypeFunction _ _ _ -> typeFunctionCall ctx tctx mod r1 typedArgs
        TypeEnumConstructor _ _ ->
          typeEnumConstructorCall ctx tctx mod r1 typedArgs

    (Throw e1) -> do
      throw $ KitError $ InternalError "Not yet implemented" (Just pos)
    (Match e1 cases (e2)) ->
      throw $ KitError $ InternalError "Not yet implemented" (Just pos)
    (InlineCall e1) ->
      throw $ KitError $ InternalError "Not yet implemented" (Just pos)

    (Field e1 (Var fieldName)) -> do
      r1 <- r e1
      case inferredType r1 of
        TypeStruct (structModPath, structName) params -> do
          structMod <- getMod ctx structModPath
          def       <- resolveLocal (modTypes structMod) structName
          case def of
            Just (TypeBinding { typeBindingType = BindingType (TypeDefinition { typeType = Struct { struct_fields = fields } }) })
              -> typeStructFieldAccess ctx tctx mod fields r1 fieldName pos
            _ -> throw $ KitError $ InternalError
              (  "Unexpected error: variable was typed as a struct, but struct "
              ++ s_unpack structName
              ++ " not found in module "
              ++ s_unpack (showModulePath structModPath)
              )
              (Just $ tPos r1)
          -- TODO
        TypePtr x ->
          -- try to auto-dereference
          r $ ep (Field (ep (PreUnop Deref e1) (tPos r1)) (Var fieldName)) pos
        _ -> throw $ KitError $ InternalError
          "Field access is not allowed on x"
          Nothing
    (Field e1 _) -> do
      throw $ KitError $ InternalError
        "Malformed AST: field access requires an identifier"
        (Just pos)

    (ArrayAccess e1 e2) ->
      throw $ KitError $ InternalError "Not yet implemented" (Just pos)

    (Cast e1 t) -> do
      r1 <- r e1
      t' <- resolveMaybeType ctx tctx mod pos t
      return $ makeExprTyped (Cast r1 t') t' pos

    (Unsafe e1) -> do
      t' <- makeTypeVar ctx pos
      let
        r1 = case expr e1 of
          Identifier i _ -> Identifier i Nothing
          _ ->
            throw $ KitError $ InternalError "Not yet implemented" (Just pos)
      return $ makeExprTyped r1 t' pos
    (BlockComment s) -> do
      return $ makeExprTyped (BlockComment s) voidType pos
    (New t args) ->
      throw $ KitError $ InternalError "Not yet implemented" (Just pos)
    (Copy e1) ->
      throw $ KitError $ InternalError "Not yet implemented" (Just pos)
    (Delete e1) ->
      throw $ KitError $ InternalError "Not yet implemented" (Just pos)
    (Move e1) ->
      throw $ KitError $ InternalError "Not yet implemented" (Just pos)
    (LexMacro s t) ->
      throw $ KitError $ InternalError "Not yet implemented" (Just pos)

    (RangeLiteral e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      resolve $ TypeEq typeClassIntegral
                       (inferredType r1)
                       "Range literal arguments must be integer types"
                       (tPos r1)
      resolve $ TypeEq typeClassIntegral
                       (inferredType r2)
                       "Range literal arguments must be integer types"
                       (tPos r2)
      return $ makeExprTyped (RangeLiteral r1 r2) TypeRange pos

    (VectorLiteral items) ->
      throw $ KitError $ InternalError "Not yet implemented" (Just pos)

    (VarDeclaration (Var vname) t (Just e1)) -> do
      varType <- resolveMaybeType ctx tctx mod pos t
      r1      <- r e1
      bindToScope (head $ tctxScopes tctx)
                  vname
                  ((newBinding (VarBinding varType) Nothing))
      resolve $ TypeEq
        (varType)
        (inferredType r1)
        "A variable's initial value must match the variable's type"
        (tPos r1)
      return $ makeExprTyped (VarDeclaration (Var vname) (varType) (Just r1))
                             varType
                             pos

    (VarDeclaration (Var vname) t Nothing) -> do
      varType <- resolveMaybeType ctx tctx mod pos t
      bindToScope (head $ tctxScopes tctx)
                  vname
                  ((newBinding (VarBinding varType) Nothing))
      return $ makeExprTyped (VarDeclaration (Var vname) varType Nothing)
                             varType
                             pos

  t' <- knownType ctx tctx mod (inferredType result)
  return $ result { inferredType = t' }

typeFunctionCall
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypedExpr
  -> [TypedExpr]
  -> IO TypedExpr
typeFunctionCall ctx tctx mod e@(TypedExpr { inferredType = TypeFunction rt argTypes isVariadic, tPos = pos }) args
  = do
  -- validate number of arguments
    if isVariadic
      then if length args < length argTypes
        then throwk $ TypingError
          (  "Expected "
          ++ (show $ length argTypes)
          ++ " or more arguments (called with "
          ++ (show $ length args)
          ++ ")"
          )
          pos
        else return ()
      else if length args /= length argTypes
        then throwk $ TypingError
          (  "Expected "
          ++ (show $ length argTypes)
          ++ " arguments (called with "
          ++ (show $ length args)
          ++ ")"
          )
          pos
        else return () -- TODO
    forM_
      (zip argTypes args)
      (\((_, argType), argValue) -> do
        t1 <- follow ctx tctx mod argType
        t2 <- knownType ctx tctx mod (inferredType argValue)
        resolveConstraint
          ctx
          tctx
          mod
          (TypeEq t1
                  t2
                  "Function arg types must match the function's declaration"
                  (tPos argValue)
          )
      )
    return $ makeExprTyped (Call e args) rt pos

typeEnumConstructorCall
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypedExpr
  -> [TypedExpr]
  -> IO TypedExpr
typeEnumConstructorCall ctx tctx mod e args = do
  return e

typeStructFieldAccess
  :: CompileContext
  -> TypeContext
  -> Module
  -> [VarDefinition Expr (Maybe TypeSpec)]
  -> TypedExpr
  -> Str
  -> Span
  -> IO TypedExpr
typeStructFieldAccess ctx tctx mod fields r fieldName pos = do
  case findStructField fields fieldName of
    Just field -> do
      t <- resolveMaybeType ctx tctx mod pos (varType field)
      return $ makeExprTyped (Field r (Var fieldName)) t pos
    Nothing -> throwk $ TypingError
      ("Struct doesn't have a field called `" ++ s_unpack fieldName ++ "`")
      pos

findStructField :: [VarDefinition a b] -> Str -> Maybe (VarDefinition a b)
findStructField (h : t) fieldName =
  if varName h == fieldName then Just h else findStructField t fieldName
findStructField [] _ = Nothing

literalConstraints :: ValueLiteral -> ConcreteType -> Span -> [TypeConstraint]
literalConstraints (BoolValue   _) s pos = [TypeEq (basicType $ BasicTypeBool) s "Bool literal must be a Bool type" pos]
literalConstraints (IntValue    _) s pos = [TypeEq typeClassNumeric s "Int literals must be a Numeric type" pos]
literalConstraints (FloatValue  _) s pos = [TypeEq typeClassNumericMixed s "Float literals must be a NumericMixed type" pos]
literalConstraints (StringValue _) s pos = [TypeEq typeClassStringy s "String literals must be a Stringy type" pos]

unopTypes :: Operator -> ConcreteType -> ConcreteType -> Span -> Maybe [TypeConstraint]
unopTypes op l x pos = case op of
  Inc        -> Just [TypeEq typeClassNumeric l "Increment operator can only be used on Numeric types" pos, TypeEq l x "An increment operation's type must match its operand" pos]
  Dec        -> Just [TypeEq typeClassNumeric l "Decrement operator can only be used on Numeric types" pos, TypeEq l x "A decrement operation's type must match its operand" pos]
  Invert     -> Just [TypeEq (TypeBasicType BasicTypeBool) l "Logical invert can only be used on Bool expressions" pos, TypeEq (basicType BasicTypeBool) x "A logical invert must yield a Bool" pos]
  InvertBits -> Just [TypeEq typeClassIntegral l "Bit invert can only be used on Integral types" pos, TypeEq l x "Bit invert must yield the same type as its operand" pos]
  Ref        -> Just [TypeEq (TypePtr l) x "Reference operator must yield a pointer to its operand's type" pos]
  Deref      -> Just [TypeEq (TypePtr x) l "Dereference operator must operate on a pointer, yielding the pointed to type" pos]
  _          -> Nothing

binopTypes
  :: Operator
  -> ConcreteType
  -> ConcreteType
  -> ConcreteType
  -> Span
  -> Maybe [TypeConstraint]
binopTypes op l r x pos = case op of
  Add        -> numericOp
  Sub        -> numericOp
  Mul        -> numericOp
  Div        -> numericOp
  Mod        -> numericOp
  Eq         -> comparisonOp
  Neq        -> comparisonOp
  Gte        -> comparisonOp
  Lte        -> comparisonOp
  LeftShift  -> numericOp
  RightShift -> numericOp
  Gt         -> comparisonOp
  Lt         -> comparisonOp
  And        -> booleanOp
  Or         -> booleanOp
  BitAnd     -> bitOp
  BitOr      -> bitOp
  BitXor     -> bitOp
  _          -> Nothing
 where
  numericOp    = Just [ TypeEq typeClassNumeric i ("Binary operator `" ++ show op ++ "` requires Numeric operands and result") pos | i <- [l, r, x] ]
  comparisonOp = Just [TypeEq l r ("Comparison operator `" ++ show op ++ "` requires operands of similar type") pos, TypeEq (TypeBasicType BasicTypeBool) x "Comparison operators must yield Bool values" pos]
  booleanOp = Just [ TypeEq (TypeBasicType BasicTypeBool) i ("Binary operator `" ++ show op ++ "` requires Bool operands and result") pos | i <- [l, r, x] ]
  bitOp        = Just [ TypeEq typeClassIntegral i ("Bitwise binary operator `" ++ show op ++ "` requires Integral operands and result") pos | i <- [l, r, x] ]
