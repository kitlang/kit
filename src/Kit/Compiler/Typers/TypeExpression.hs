module Kit.Compiler.Typers.TypeExpression where

import Control.Monad
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.Base
import Kit.Compiler.Unify
import Kit.Error
import Kit.Parser
import Kit.Str

typeMaybeExpr
  :: CompileContext
  -> TypeContext
  -> Module
  -> Maybe Expr
  -> IO (Maybe TypedExpr)
typeMaybeExpr ctx tctx mod e = case e of
  Just ex -> do
    result <- typeExpr ctx tctx mod ex
    return $ Just result
  Nothing -> return Nothing

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
            Just (Binding { bindingType = VarBinding, bindingConcrete = t, bindingNameMangling = mangle })
              -> return $ makeExprTyped (Identifier v mangle) t pos
            Just (Binding { bindingType = FunctionBinding, bindingConcrete = TypeFunction f args variadic, bindingNameMangling = mangle })
              -> return $ makeExprTyped (Identifier v mangle)
                                        (TypeFunction f args variadic)
                                        pos
            Just (Binding { bindingType = EnumConstructor, bindingConcrete = TypeEnumConstructor t args })
              ->
              -- TODO: handle type params
                 return $ makeExprTyped (Identifier v Nothing) t' pos
             where
              t' = if args == []
                then (TypeEnum t [])
                else (TypeEnumConstructor t args)
            _ -> throwk
              $ TypingError ("Unknown identifier: " ++ (s_unpack vname)) pos
        MacroVar vname (Just t) -> do
          macroType <- resolveType ctx tctx mod t
          return $ makeExprTyped
            (Identifier (MacroVar vname (Just t)) Nothing)
            macroType
            pos
        MacroVar vname Nothing -> throwk $ TypingError
          (  "To use macro variable $"
          ++ (s_unpack vname)
          ++ " outside of a rewrite rule, it needs a type annotation:\n\n    ${"
          ++ (s_unpack vname)
          ++ ": Type}"
          )
          pos

    (TypeAnnotation e1 t) -> do
      r1 <- r e1
      t  <- resolveMaybeType ctx tctx mod pos t
      resolve $ TypeEq
        (inferredType r1)
        t
        "Annotated expressions must match their type annotation"
        (tPos r1)
      return r1 { inferredType = t }

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
      resolve $ TypeEq (inferredType r1)
                       (inferredType r2)
                       "Both sides of an assignment must unify"
                       pos
      return $ makeExprTyped (Binop Assign r1 r2) (inferredType r1) pos
    (Binop (AssignOp op) e1 e2) | (op == And) || (op == Or) -> do
      r1 <- r e1
      r2 <- r e2
      resolve $ TypeEq (inferredType r1)
                       (inferredType r2)
                       "Both sides of an assignment must unify"
                       pos
      return $ makeExprTyped
        (Binop Assign r1 (makeExprTyped (Binop op r1 r2) (inferredType r1) pos))
        (inferredType r1)
        pos
    (Binop (AssignOp x) e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      -- FIXME: this isn't right
      resolve $ TypeEq (inferredType r1)
                       (inferredType r2)
                       "FIXME: this isn't right"
                       pos
      return $ makeExprTyped (Binop (AssignOp x) r1 r2) (inferredType r1) pos
    (Binop op e1 e2) -> do
      r1     <- r e1
      r2     <- r e2
      lMixed <- unify ctx tctx mod (inferredType r1) (typeClassNumericMixed)
      rMixed <- unify ctx tctx mod (inferredType r2) (typeClassNumericMixed)
      tv     <- makeTypeVar ctx pos
      case
          binopTypes op
                     (inferredType r1)
                     (inferredType r2)
                     tv
                     (lMixed == TypeConstraintSatisfied)
                     (rMixed == TypeConstraintSatisfied)
                     pos
        of
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
      case expr e2 of
        RangeLiteral _ _ -> return ()
        _                -> resolve $ TypeEq
          (typeClassIterable tv)
          (inferredType r2)
          "For statements must iterate over an Iterable type"
          (tPos r2)
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
      resolve $ TypeEq (inferredType r1)
                       (basicType BasicTypeBool)
                       "A while condition must be a Bool"
                       (tPos r1)
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
      resolve $ TypeEq (inferredType r1)
                       (basicType BasicTypeBool)
                       "An if condition must be a Bool"
                       (tPos r1)
      resolve $ TypeEq
        (inferredType r2)
        (inferredType r3)
        "In an if expression with an else clause, both clauses must have the same type"
        pos
      resolve $ TypeEq (inferredType r2)
                       (tv)
                       "The type of an if expression must match its clauses"
                       (tPos r2)
      return $ makeExprTyped (If r1 r2 (Just r3)) (tv) pos
    (If e1 e2 Nothing) -> do
      r1    <- r e1
      scope <- newScope
      r2    <- typeExpr ctx
                        (tctx { tctxScopes = scope : (tctxScopes tctx) })
                        mod
                        e2
      resolve $ TypeEq (inferredType r1)
                       (basicType BasicTypeBool)
                       "An if condition must be a Bool"
                       (tPos r1)
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
      throwk $ InternalError "Not yet implemented" (Just pos)
    (Match e1 cases (e2)) ->
      throwk $ InternalError "Not yet implemented" (Just pos)
    (InlineCall e1) -> throwk $ InternalError "Not yet implemented" (Just pos)

    (Field e1 (Var fieldName)) -> do
      r1 <- r e1
      case inferredType r1 of
        TypeStruct (structModPath, structName) params -> do
          structMod <- getMod ctx structModPath
          def       <- resolveLocal (modDefinitions structMod) structName
          case def of
            Just (DefinitionType (TypeDefinition { typeType = Struct { structFields = fields } }))
              -> typeStructFieldAccess ctx tctx mod fields r1 fieldName pos
            _ -> throwk $ InternalError
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
        x -> throwk $ InternalError
          ("Field access is not allowed on " ++ show x)
          Nothing
    (Field e1 _) -> do
      throwk $ InternalError
        "Malformed AST: field access requires an identifier"
        (Just pos)

    (ArrayAccess e1 e2) ->
      throwk $ InternalError "Not yet implemented" (Just pos)

    (Cast e1 t) -> do
      r1 <- r e1
      t' <- resolveMaybeType ctx tctx mod pos t
      return $ makeExprTyped (Cast r1 t') t' pos

    (Unsafe e1) -> do
      t' <- makeTypeVar ctx pos
      let r1 = case expr e1 of
            Identifier i _ -> Identifier i Nothing
            _ -> throwk $ InternalError "Not yet implemented" (Just pos)
      return $ makeExprTyped r1 t' pos
    (BlockComment s) -> do
      return $ makeExprTyped (BlockComment s) voidType pos
    (New t args) -> throwk $ InternalError "Not yet implemented" (Just pos)
    (Copy e1) -> throwk $ InternalError "Not yet implemented" (Just pos)
    (Delete e1) -> throwk $ InternalError "Not yet implemented" (Just pos)
    (Move e1) -> throwk $ InternalError "Not yet implemented" (Just pos)
    (LexMacro s t) -> throwk $ InternalError "Not yet implemented" (Just pos)

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
      throwk $ InternalError "Not yet implemented" (Just pos)

    (VarDeclaration (Var vname) t init) -> do
      varType <- resolveMaybeType ctx tctx mod pos t
      init'   <- case init of
        Just e1 -> do
          r1 <- r e1
          resolve $ TypeEq
            (varType)
            (inferredType r1)
            "A variable's initial value must match the variable's type"
            (tPos r1)
          return $ Just r1
        Nothing -> return Nothing
      existing <- resolveLocal (head $ tctxScopes tctx) vname
      case existing of
        Just (Binding { bindingPos = pos' }) ->
          throwk $ DuplicateDeclarationError (modPath mod) vname pos pos'
        _ -> bindToScope (head $ tctxScopes tctx)
                         vname
                         (newBinding VarBinding varType Nothing pos)
      return $ makeExprTyped (VarDeclaration (Var vname) (varType) init')
                             varType
                             pos

    (StructInit (Just t) fields) -> do
      structType <- resolveType ctx tctx mod t
      case structType of
        TypeStruct tp@(mp, name) params -> do
          owningModule <- getMod ctx mp
          structDef    <- resolveLocal (modDefinitions owningModule) name
          case structDef of
            Just (DefinitionType (TypeDefinition { typeType = Struct { structFields = structFields } }))
              -> do
                let providedNames = map fst fields
                let fieldNames    = map varName structFields
                let extraNames    = providedNames \\ fieldNames
                -- TODO: check for duplicate fields
                -- check for extra fields
                if not (null extraNames)
                  then throwk $ BasicError
                    (  "Struct "
                    ++ s_unpack (showTypePath tp)
                    ++ " has the following extra fields:\n\n"
                    ++ intercalate
                         "\n"
                         [ "  - `" ++ s_unpack name ++ "`"
                         | name <- extraNames
                         ]
                    ++ "\n\nRemove these fields or correct any typos."
                    )
                    (Just pos)
                  else return ()
                let nonProvidedNames = fieldNames \\ providedNames
                typedFields <- forM
                  (structFields)
                  (\field -> do
                    fieldType <- resolveMaybeType ctx
                                                  tctx
                                                  mod
                                                  pos
                                                  (varType field)
                    let provided =
                          find (\(name, _) -> name == varName field) fields
                    case provided of
                      Just (name, value) -> return ((name, value), fieldType)
                      Nothing            -> case varDefault field of
                        Just fieldDefault ->
                          return ((varName field, fieldDefault), fieldType)
                        Nothing -> throwk $ BasicError
                          (  "Struct "
                          ++ s_unpack (showTypePath tp)
                          ++ " is missing field "
                          ++ s_unpack (varName field)
                          ++ ", and no default value is provided."
                          )
                          (Just pos)
                  )
                typedFields <- forM
                  typedFields
                  (\((name, expr), fieldType) -> do
                    r1 <- r expr
                    resolve $ TypeEq
                      (inferredType r1)
                      fieldType
                      "Provided struct field values must match the declared struct field type"
                      (tPos r1)
                    return (name, r1)
                  )
                return $ makeExprTyped (StructInit structType typedFields)
                                       structType
                                       pos
        _ -> throwk
          $ BasicError ("Type " ++ (show t) ++ " is not a struct") (Just pos)


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
literalConstraints (BoolValue _) s pos =
  [TypeEq (basicType $ BasicTypeBool) s "Bool literal must be a Bool type" pos]
literalConstraints (IntValue _) s pos =
  [TypeEq typeClassNumeric s "Int literals must be a Numeric type" pos]
literalConstraints (FloatValue _) s pos =
  [ TypeEq typeClassNumericMixed
           s
           "Float literals must be a NumericMixed type"
           pos
  ]
literalConstraints (StringValue _) s pos =
  [TypeEq typeClassStringy s "String literals must be a Stringy type" pos]

unopTypes
  :: Operator -> ConcreteType -> ConcreteType -> Span -> Maybe [TypeConstraint]
unopTypes op l x pos = case op of
  Inc -> Just
    [ TypeEq typeClassNumeric
             l
             "Increment operator can only be used on Numeric types"
             pos
    , TypeEq l x "An increment operation's type must match its operand" pos
    ]
  Dec -> Just
    [ TypeEq typeClassNumeric
             l
             "Decrement operator can only be used on Numeric types"
             pos
    , TypeEq l x "A decrement operation's type must match its operand" pos
    ]
  Invert -> Just
    [ TypeEq (TypeBasicType BasicTypeBool)
             l
             "Logical invert can only be used on Bool expressions"
             pos
    , TypeEq (basicType BasicTypeBool)
             x
             "A logical invert must yield a Bool"
             pos
    ]
  InvertBits -> Just
    [ TypeEq typeClassIntegral
             l
             "Bit invert can only be used on Integral types"
             pos
    , TypeEq l x "Bit invert must yield the same type as its operand" pos
    ]
  Ref -> Just
    [ TypeEq (TypePtr l)
             x
             "Reference operator must yield a pointer to its operand's type"
             pos
    ]
  Deref -> Just
    [ TypeEq
        (TypePtr x)
        l
        "Dereference operator must operate on a pointer, yielding the pointed to type"
        pos
    ]
  _ -> Nothing

binopTypes
  :: Operator
  -> ConcreteType
  -> ConcreteType
  -> ConcreteType
  -> Bool
  -> Bool
  -> Span
  -> Maybe [TypeConstraint]
binopTypes op l r x lMixed rMixed pos = case op of
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
  numericOp =
    Just
      $  (if (rMixed || rMixed)
           then
             [ TypeEq
                 typeClassNumericMixed
                 x
                 ("Binary operator `"
                 ++ show op
                 ++ "` requires a NumericMixed result if either operand is NumericMixed"
                 )
                 pos
             ]
           else []
         )
      ++ [ TypeEq
             typeClassNumeric
             i
             (  "Binary operator `"
             ++ show op
             ++ "` requires Numeric operands and result"
             )
             pos
         | i <- [l, r, x]
         ]
  comparisonOp = Just
    [ TypeEq
      l
      r
      (  "Comparison operator `"
      ++ show op
      ++ "` requires operands of similar type"
      )
      pos
    , TypeEq (TypeBasicType BasicTypeBool)
             x
             "Comparison operators must yield Bool values"
             pos
    ]
  booleanOp = Just
    [ TypeEq
        (TypeBasicType BasicTypeBool)
        i
        ("Binary operator `" ++ show op ++ "` requires Bool operands and result"
        )
        pos
    | i <- [l, r, x]
    ]
  bitOp = Just
    [ TypeEq
        typeClassIntegral
        i
        (  "Bitwise binary operator `"
        ++ show op
        ++ "` requires Integral operands and result"
        )
        pos
    | i <- [l, r, x]
    ]
