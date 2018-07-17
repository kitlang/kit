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
      unification <- unify ctx tctx mod (resolvedReturnType) (TypeBasicType BasicTypeVoid)
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
  let resolve pos constraint = resolveConstraint ctx tctx mod pos constraint
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
      mapM_ (resolve pos) $ literalConstraints l typeVar
      return $ makeExprTyped (Literal l) typeVar pos

    (This) -> do
      case tctxThis tctx of
        Just t  -> return $ makeExprTyped This t pos
        Nothing -> throw $ Errs
          [errp TypingError ("`this` can only be used in methods") (Just pos)]

    (Self) -> do
      case tctxSelf tctx of
        Just t  -> return $ makeExprTyped Self t pos
        Nothing -> throw $ Errs
          [errp TypingError ("`Self` can only be used in methods") (Just pos)]

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
            Nothing ->
              throw
                $ Errs
                    [ errp TypingError
                           ("Unknown identifier: " ++ (s_unpack vname))
                           (Just pos)
                    ]
        MacroVar vname _ -> throw $ Errs
          [ errp
              TypingError
              (  "Macro variable $"
              ++ (s_unpack vname)
              ++ " can only be used in a rewrite rule"
              )
              (Just pos)
          ]

    (TypeAnnotation e1 t) -> do
      r1 <- r e1
      t  <- resolveMaybeType ctx tctx mod pos t
      resolve (tPos r1) $ TypeEq (inferredType r1) t
      return r1

    (PreUnop op e1) -> do
      r1 <- r e1
      tv <- makeTypeVar ctx pos
      case unopTypes op (inferredType r1) tv of
        Just constraints -> mapM_ (resolve pos) constraints
        Nothing          -> return () -- TODO
      return $ makeExprTyped (PreUnop op r1) tv pos

    (PostUnop op e1) -> do
      r1 <- r e1
      tv <- makeTypeVar ctx pos
      case unopTypes op (inferredType r1) tv of
        Just constraints -> mapM_ (resolve pos) constraints
        Nothing          -> return () -- TODO
      return $ makeExprTyped (PostUnop op r1) tv pos

    (Binop Assign e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      resolve pos $ TypeEq (inferredType r1) (inferredType r2)
      return $ makeExprTyped (Binop Assign r1 r2) (inferredType r1) pos
    (Binop (AssignOp op) e1 e2) | (op == And) || (op == Or) -> do
      r1 <- r e1
      r2 <- r e2
      resolve pos $ TypeEq (inferredType r1) (inferredType r2)
      return $ makeExprTyped
        (Binop Assign r1 (makeExprTyped (Binop op r1 r2) (inferredType r1) pos))
        (inferredType r1)
        pos
    (Binop (AssignOp x) e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      -- FIXME: this isn't right
      resolve pos $ TypeEq (inferredType r1) (inferredType r2)
      return $ makeExprTyped (Binop (AssignOp x) r1 r2) (inferredType r1) pos
    (Binop op e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      tv <- makeTypeVar ctx pos
      case binopTypes op (inferredType r1) (inferredType r2) tv of
        Just constraints -> mapM_ (resolve pos) constraints
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
      resolve (tPos r2) $ TypeEq (typeClassIterable tv) (inferredType r2)
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
      resolve (tPos r1) $ TypeEq (inferredType r1) (basicType BasicTypeBool)
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
      resolve (tPos r1) $ TypeEq (inferredType r1) (basicType BasicTypeBool)
      resolve pos $ TypeEq (inferredType r2) (inferredType r3)
      resolve (tPos r2) $ TypeEq (inferredType r2) (tv)
      return $ makeExprTyped (If r1 r2 (Just r3)) (tv) pos
    (If e1 e2 Nothing) -> do
      r1    <- r e1
      scope <- newScope
      r2    <- typeExpr ctx
                        (tctx { tctxScopes = scope : (tctxScopes tctx) })
                        mod
                        e2
      resolve (tPos r1) $ TypeEq (inferredType r1) (basicType BasicTypeBool)
      return $ makeExprTyped (If r1 r2 Nothing) voidType pos

    (Continue) -> do
      if (tctxLoopCount tctx > 0)
        then return $ makeExprTyped (Continue) voidType pos
        else throw $ Errs [err TypingError "Can't `continue` outside of a loop"]
    (Break) -> do
      if (tctxLoopCount tctx > 0)
        then return $ makeExprTyped (Break) voidType pos
        else throw $ Errs [err TypingError "Can't `break` outside of a loop"]

    (Return e1) -> do
      case (tctxReturnType tctx, e1) of
        (Just rt, Just e1) -> do
          r1 <- r e1
          resolve (tPos r1) $ TypeEq (rt) (inferredType r1)
          return $ makeExprTyped (Return $ Just r1) voidType pos
        (Just rt, Nothing) -> do
          resolve pos $ TypeEq voidType (rt)
          return $ makeExprTyped (Return Nothing) voidType pos
        (Nothing, _) -> throw
          $ Errs [err TypingError "Can't `return` outside of a function"]

    (Call e1 args) -> do
      r1        <- r e1
      typedArgs <- mapM r args
      case inferredType r1 of
        TypeFunction _ _ _ -> typeFunctionCall ctx tctx mod r1 typedArgs
        TypeEnumConstructor _ _ ->
          typeEnumConstructorCall ctx tctx mod r1 typedArgs

    (Throw e1) -> do
      throw $ Errs [err InternalError "Not yet implemented"]
    (Match e1 cases (e2)) ->
      throw $ Errs [err InternalError "Not yet implemented"]
    (InlineCall e1) -> throw $ Errs [err InternalError "Not yet implemented"]

    (Field e1 (Var fieldName)) -> do
      r1 <- r e1
      case inferredType r1 of
        TypeStruct (structModPath, structName) params -> do
          structMod <- getMod ctx structModPath
          def       <- resolveLocal (modTypes structMod) structName
          case def of
            Just (TypeBinding {typeBindingType = BindingType (TypeDefinition { typeType = Struct { struct_fields = fields } })})
              -> typeStructFieldAccess ctx tctx mod fields r1 fieldName pos
            _ -> throw $ Errs
              [ errp
                  InternalError
                  ("Unexpected error: variable was typed as a struct, but struct "
                  ++ s_unpack structName
                  ++ " not found in module "
                  ++ s_unpack (showModulePath structModPath)
                  )
                  (Just $ tPos r1)
              ]
          -- TODO
        TypePtr x ->
          -- try to auto-dereference
          r $ ep (Field (ep (PreUnop Deref e1) (tPos r1)) (Var fieldName)) pos
        _ ->
          throw $ Errs [err InternalError "Field access is not allowed on x"]
    (Field e1 _) -> do
      throw $ Errs
        [ errp TypingError
               "Malformed AST: field access requires an identifier"
               (Just pos)
        ]

    (ArrayAccess e1 e2) ->
      throw $ Errs [err InternalError "Not yet implemented"]

    (Cast e1 t) -> do
      r1 <- r e1
      t' <- resolveMaybeType ctx tctx mod pos t
      return $ makeExprTyped (Cast r1 t') t' pos

    (Unsafe e1) -> do
      t' <- makeTypeVar ctx pos
      let r1 = case expr e1 of
            Identifier i _ -> Identifier i Nothing
            _ -> throw $ Errs [err InternalError "Not yet implemented"]
      return $ makeExprTyped r1 t' pos
    (BlockComment s) -> do
      return $ makeExprTyped (BlockComment s) voidType pos
    (New t args) -> throw $ Errs [err InternalError "Not yet implemented"]
    (Copy e1) -> throw $ Errs [err InternalError "Not yet implemented"]
    (Delete e1) -> throw $ Errs [err InternalError "Not yet implemented"]
    (Move e1) -> throw $ Errs [err InternalError "Not yet implemented"]
    (LexMacro s t) -> throw $ Errs [err InternalError "Not yet implemented"]

    (RangeLiteral e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      resolve (tPos r1) $ TypeEq typeClassIntegral (inferredType r1)
      resolve (tPos r2) $ TypeEq typeClassIntegral (inferredType r2)
      return $ makeExprTyped (RangeLiteral r1 r2) TypeRange pos

    (VectorLiteral items) ->
      throw $ Errs [err InternalError "Not yet implemented"]

    (VarDeclaration (Var vname) t (Just e1)) -> do
      varType <- resolveMaybeType ctx tctx mod pos t
      r1      <- r e1
      bindToScope (head $ tctxScopes tctx)
                  vname
                  ((newBinding (VarBinding varType) Nothing))
      resolve (tPos r1) $ TypeEq (varType) (inferredType r1)
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
        then throw $ Errs
          [ errp
              TypingError
              (  "Expected "
              ++ (show $ length argTypes)
              ++ " or more arguments (called with "
              ++ (show $ length args)
              ++ ")"
              )
              (Just pos)
          ]
        else return ()
      else if length args /= length argTypes
        then throw $ Errs
          [ errp
              TypingError
              (  "Expected "
              ++ (show $ length argTypes)
              ++ " arguments (called with "
              ++ (show $ length args)
              ++ ")"
              )
              (Just pos)
          ]
        else return () -- TODO
    forM_
      (zip argTypes args)
      (\((_, argType), argValue) -> do
        t1 <- follow ctx tctx mod argType
        t2 <- knownType ctx tctx mod (inferredType argValue)
        resolveConstraint ctx tctx mod (tPos argValue) (TypeEq t1 t2)
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
    Nothing -> throw $ Errs
      [ errp
          TypingError
          ("Struct doesn't have a field called `" ++ s_unpack fieldName ++ "`")
          (Just $ pos)
      ]

findStructField :: [VarDefinition a b] -> Str -> Maybe (VarDefinition a b)
findStructField (h : t) fieldName =
  if varName h == fieldName then Just h else findStructField t fieldName
findStructField [] _ = Nothing

literalConstraints :: ValueLiteral -> ConcreteType -> [TypeConstraint]
literalConstraints (BooIdentifier _) s = [TypeEq s (basicType $ BasicTypeBool)]
literalConstraints (IntValue      _) s = [TypeEq typeClassNumeric s]
literalConstraints (FloatValue    _) s = [TypeEq typeClassNumericMixed s]
literalConstraints (StringValue   _) s = [TypeEq typeClassStringy s]

opTypes
  :: Operator
  -> ConcreteType
  -> [ConcreteType]
  -> (ConcreteType, [TypeConstraint])
opTypes op result operands =
  (result, [ TypeEq typeClassNumeric operand | operand <- (result : operands) ])

unopTypes :: Operator -> ConcreteType -> ConcreteType -> Maybe [TypeConstraint]
unopTypes op l x = case op of
  Inc        -> Just [TypeEq typeClassNumeric l, TypeEq l x]
  Dec        -> Just [TypeEq typeClassNumeric l, TypeEq l x]
  Invert     -> Just [TypeEq (TypeBasicType BasicTypeBool) l, TypeEq l x]
  InvertBits -> Just [TypeEq typeClassIntegral l, TypeEq l x]
  Ref        -> Just [TypeEq (TypePtr l) x]
  Deref      -> Just [TypeEq (TypePtr x) l]
  _          -> Nothing

binopTypes
  :: Operator
  -> ConcreteType
  -> ConcreteType
  -> ConcreteType
  -> Maybe [TypeConstraint]
binopTypes op l r x = case op of
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
  numericOp    = Just [ TypeEq typeClassNumeric i | i <- [l, r, x] ]
  comparisonOp = Just [TypeEq l r, TypeEq (TypeBasicType BasicTypeBool) x]
  booleanOp = Just [ TypeEq (TypeBasicType BasicTypeBool) i | i <- [l, r, x] ]
  bitOp        = Just [ TypeEq typeClassIntegral i | i <- [l, r, x] ]
