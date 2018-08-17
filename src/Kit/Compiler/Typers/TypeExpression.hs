module Kit.Compiler.Typers.TypeExpression where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TermRewrite
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.AutoRefDeref
import Kit.Compiler.Typers.Base
import Kit.Compiler.Typers.ConvertExpr
import Kit.Compiler.Unify
import Kit.Error
import Kit.HashTable
import Kit.Parser
import Kit.Str

typeMaybeExpr
  :: CompileContext
  -> TypeContext
  -> Module
  -> Maybe TypedExpr
  -> IO (Maybe TypedExpr)
typeMaybeExpr ctx tctx mod e = case e of
  Just ex -> do
    result <- typeExpr ctx tctx mod ex
    return $ Just result
  Nothing -> return Nothing

partialTyping ex et e = return $ ex { tExpr = et, tError = Just $ KitError e }

{-
  Converts a tree of untyped AST to typed AST.

  The compile process is roughly:

    untyped AST (Expr)
    -> typed AST (TypedExpr) <- we are generating this
    -> IR (IrExpr)
    -> generated code

  After typing a subexpression, rewrite rules may take effect and trigger
  an early exit.
-}
typeExpr :: CompileContext -> TypeContext -> Module -> TypedExpr -> IO TypedExpr
typeExpr ctx tctx mod ex@(TypedExpr { tExpr = et, tPos = pos }) = do
  let r = typeExpr ctx tctx mod
  let resolve constraint = resolveConstraint ctx tctx mod constraint
  let unknownTyped x = makeExprTyped x (TypeBasicType BasicTypeUnknown) pos
  let tryRewrite x y = do
        result <- foldM
          (\acc rule -> case acc of
            Just x  -> return $ Just x
            Nothing -> rewriteExpr
              ctx
              tctx
              mod
              rule
              x
              (\tctx ex -> do
                body <- convertExpr ctx tctx mod ex
                typeExpr ctx tctx mod body
              )
          )
          Nothing
          [ rule | ruleset <- tctxRules tctx, rule <- ruleSetRules ruleset ]
        case result of
          Just x  -> return x
          Nothing -> y
  let failTyping e = partialTyping ex et e

  result <- case et of
    (Block children) -> do
      blockScope <- newScope (scopeNamespace $ head $ tctxScopes tctx)
      let tctx' = tctx { tctxScopes = blockScope : tctxScopes tctx }
      typedChildren <- forM children $ \child -> do
        tmp    <- newIORef []
        result <- typeExpr ctx (tctx' { tctxTemps = Just tmp }) mod child
        temps  <- readIORef tmp
        return $ result { tTemps = temps }
      return $ makeExprTyped
        (Block typedChildren)
        (if children == [] then voidType else inferredType $ last typedChildren)
        pos

    (Using using e1) -> do
      tctx' <- foldM
        (\c use -> do
          using' <- case use of
            UsingImplicit x ->
              (do
                x' <- r x
                return $ UsingImplicit x'
              )
            _ -> return use
          addUsing ctx c mod using'
        )
        tctx
        using
      r1 <- typeExpr ctx tctx' mod e1
      return $ makeExprTyped (Using using r1) (inferredType $ r1) pos

    (Meta m e1) -> do
      r1 <- r e1
      return $ makeExprTyped (Meta m r1) (inferredType r1) pos

    (Literal l) -> do
      mapM_ resolve $ literalConstraints l (inferredType ex) pos
      return ex

    (This) -> do
      case tctxThis tctx of
        Just t -> return $ makeExprTyped This t pos
        Nothing ->
          failTyping $ TypingError ("`this` can only be used in methods") pos

    (Self) -> do
      case tctxSelf tctx of
        Just t -> return $ makeExprTyped Self (TypeTypeOf t) pos
        Nothing ->
          failTyping $ TypingError ("`Self` can only be used in methods") pos

    (Identifier v namespace) -> do
      tryRewrite
        (unknownTyped $ Identifier v namespace)
        (case v of
          Var vname -> do
            binding <- resolveVar ctx (tctxScopes tctx) mod vname
            case binding of
              Just binding -> do
                x <- typeVarBinding ctx vname binding pos
                return $ x { tIsLvalue = True }
              Nothing -> failTyping
                $ TypingError ("Unknown identifier: " ++ s_unpack vname) pos
          MacroVar vname t -> do
            case find (\(name, _) -> name == vname) (tctxMacroVars tctx) of
              Just (name, expr) -> r expr
              Nothing           -> return ex
        )

    (TypeAnnotation e1 t) -> do
      r1 <- r e1
      tryRewrite (unknownTyped $ TypeAnnotation r1 t) $ do
        resolve $ TypeEq
          (inferredType r1)
          t
          "Annotated expressions must match their type annotation"
          (tPos r1)
        return $ r1 { inferredType = t }

    (PreUnop op e1) -> do
      r1 <- r e1
      tryRewrite (unknownTyped $ PreUnop op r1) $ do
        case unopTypes op (inferredType r1) (inferredType ex) pos of
          Just constraints -> mapM_ resolve constraints
          Nothing          -> return () -- TODO
        return $ makeExprTyped (PreUnop op r1) (inferredType ex) pos

    (PostUnop op e1) -> do
      r1 <- r e1
      tryRewrite (unknownTyped $ PostUnop op r1) $ do
        case unopTypes op (inferredType r1) (inferredType ex) pos of
          Just constraints -> mapM_ resolve constraints
          Nothing          -> return () -- TODO
        return $ makeExprTyped (PostUnop op r1) (inferredType ex) pos

    (Binop Assign e1 e2) -> do
      r2 <- r e2
      tryRewrite (unknownTyped $ Binop Assign e1 r2) $ do
        case tExpr e1 of
          TupleInit t -> do
            case inferredType r2 of
              TypeTuple t2 -> if length t == length t2
                then do
                  forM_ (zip t t2) $ \(a, b) -> do
                    resolve $ TypeEq
                      (inferredType a)
                      b
                      "Tuple contents must match variables in tuple assignment"
                      pos

                  tupleExpr <- if tIsLvalue r2
                    then return r2
                    else do
                      tmp <- makeTmpVar (head $ tctxScopes tctx)
                      let tmpAssignment = makeExprTyped
                            (VarDeclaration (Var tmp)
                                            (inferredType r2)
                                            (Just r2)
                            )
                            (inferredType r2)
                            (tPos r2)
                      case tctxTemps tctx of
                        Just v -> do
                          modifyIORef v (\val -> val ++ [tmpAssignment])
                      return
                        $ (makeExprTyped (Identifier (Var tmp) [])
                                         (inferredType r2)
                                         pos
                          )

                  let boundSlots = filter (\(i, (a, b)) -> tExpr a /= Identifier Hole []) (zip [0 ..] (zip t t2))
                  slots <- forM boundSlots $ \(i, (a, b)) -> do
                    let e1 = makeExprTyped
                                                  (Binop
                                                    Assign
                                                    a
                                                    (makeExprTyped (TupleSlot tupleExpr i) b pos)
                                                  )
                                                  b
                                                  (tPos a)
                    case tExpr a of
                      Identifier _ _ -> return e1
                      _ -> r e1

                  return
                    $ (makeExprTyped
                        (Block slots
                        )
                        (inferredType r2)
                        pos
                      )
                else failTyping $ TypingError
                  "Tuples can only be assigned to tuples of the same size"
                  pos
              _ -> failTyping $ TypingError
                "Tuples can only be assigned to matching tuples"
                pos
          _ -> do
            r1        <- r e1
            converted <- autoRefDeref ctx
                                      tctx
                                      mod
                                      (inferredType r1)
                                      (inferredType r2)
                                      r2
                                      []
                                      r2
            resolve $ TypeEq (inferredType r1)
                             (inferredType converted)
                             "Both sides of an assignment must unify"
                             pos
            return $ makeExprTyped (Binop Assign r1 converted)
                                   (inferredType r1)
                                   pos
    (Binop (AssignOp op) e1 e2) | (op == And) || (op == Or) -> do
      r1 <- r e1
      r2 <- r e2
      tryRewrite (unknownTyped $ Binop (AssignOp op) r1 r2) $ do
        resolve $ TypeEq (inferredType r1)
                         (inferredType r2)
                         "Both sides of an assignment must unify"
                         pos
        return $ makeExprTyped
          (Binop Assign
                 r1
                 (makeExprTyped (Binop op r1 r2) (inferredType r1) pos)
          )
          (inferredType r1)
          pos
    (Binop op@(AssignOp x) e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      tryRewrite (unknownTyped $ Binop op r1 r2) $ do
        -- FIXME: this isn't right
        resolve $ TypeEq (inferredType r1)
                         (inferredType r2)
                         "FIXME: this isn't right"
                         pos
        return $ makeExprTyped (Binop op r1 r2) (inferredType r1) pos
    (Binop op@(Custom s) e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      tryRewrite (unknownTyped $ Binop op r1 r2) $ do
        partialTyping ex (Binop op r1 r2) $ BasicError
          (  "Custom operator `"
          ++ s_unpack s
          ++ "` can only be used as part of a rewrite rule"
          )
          (Just pos)
    (Binop op e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      tryRewrite (unknownTyped $ Binop op r1 r2) $ do
        lMixed <- unify ctx tctx mod (inferredType r1) (typeClassNumericMixed)
        rMixed <- unify ctx tctx mod (inferredType r2) (typeClassNumericMixed)
        tv     <- makeTypeVar ctx pos
        case
            binopTypes op
                       (inferredType r1)
                       (inferredType r2)
                       tv
                       (lMixed /= Nothing)
                       (rMixed /= Nothing)
                       pos
          of
            Just constraints -> mapM_ resolve constraints
            Nothing          -> return () -- TODO
        return $ makeExprTyped (Binop op r1 r2) tv pos

    (For (TypedExpr { tExpr = Identifier v _, tPos = pos1 }) e2 e3) -> do
      r2    <- r e2
      scope <- newScope (modPath mod)
      r3    <- typeExpr
        ctx
        (tctx { tctxScopes    = scope : (tctxScopes tctx)
              , tctxLoopCount = (tctxLoopCount tctx) + 1
              }
        )
        mod
        e3
      tv <- makeTypeVar ctx pos
      case tExpr e2 of
        RangeLiteral _ _ -> return ()
        _                -> resolve $ TypeEq
          (typeClassIterable tv)
          (inferredType r2)
          "For statements must iterate over an Iterable type"
          (tPos r2)
      return $ makeExprTyped
        (For (makeExprTyped (Identifier v []) (TypeIdentifier tv) pos1) r2 r3)
        voidType
        pos

    (While e1 e2 d) -> do
      r1    <- r e1
      scope <- newScope (modPath mod)
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
      return $ makeExprTyped (While r1 r2 d) voidType pos

    (If e1 e2 (Just e3)) -> do
      r1     <- r e1
      scope1 <- newScope (modPath mod)
      r2     <- typeExpr ctx
                         (tctx { tctxScopes = scope1 : (tctxScopes tctx) })
                         mod
                         e2
      scope2 <- newScope (modPath mod)
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
      -- FIXME: scope
      scope <- newScope (modPath mod)
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
        else failTyping $ TypingError "Can't `continue` outside of a loop" pos

    (Break) -> do
      if (tctxLoopCount tctx > 0)
        then return $ makeExprTyped (Break) voidType pos
        else failTyping $ TypingError "Can't `break` outside of a loop" pos

    (Return e1) -> do
      r1 <- typeMaybeExpr ctx tctx mod e1
      case (tctxReturnType tctx, r1) of
        (Just rt, Just r1) -> do
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
          failTyping $ TypingError "Can't `return` outside of a function" pos

    (Call e1 args) -> do
      r1 <- r e1
      let implicits = tImplicits r1 ++ tctxImplicits tctx
      typedArgs <- mapM r args
      tryRewrite (unknownTyped $ Call r1 typedArgs) $ case inferredType r1 of
        TypeFunction _ _ _ ->
          typeFunctionCall ctx tctx mod r1 implicits typedArgs
        TypeEnumConstructor tp discriminant argTypes -> typeEnumConstructorCall
          ctx
          tctx
          mod
          r1
          typedArgs
          tp
          discriminant
          argTypes
        x -> partialTyping ex (Call r1 typedArgs)
          $ TypingError ("Type " ++ show x ++ " is not callable") pos

    (Throw e1) -> do
      throwk $ InternalError "Not yet implemented" (Just pos)
    (Match e1 cases (e2)) ->
      throwk $ InternalError "Not yet implemented" (Just pos)
    (InlineCall e1) -> throwk $ InternalError "Not yet implemented" (Just pos)

    (Field e1 (Var fieldName)) -> do
      r1 <- r e1
      tryRewrite (unknownTyped $ Field r1 (Var fieldName)) $ do
        let
          typeFieldAccess t fieldName = do
            case t of
              TypeInstance (modPath, typeName) params -> do
                defMod  <- getMod ctx modPath
                binding <- resolveLocal (modScope defMod) typeName
                case binding of
                  Just (Binding { bindingType = TypeBinding def@(TypeDefinition { typeSubtype = subtype }) })
                    -> do
                      localScope <- getSubScope (modScope mod) [typeName]
                      binding    <- resolveLocal localScope fieldName
                      case binding of
                        Just x -> do
                          typed <- typeVarBinding ctx fieldName x pos
                          return $ typed { tImplicits = r1 : tImplicits typed }
                        _ -> case subtype of
                          Struct { structFields = fields } -> do
                            result <- typeStructUnionFieldAccess ctx
                                                                 tctx
                                                                 mod
                                                                 fields
                                                                 r1
                                                                 fieldName
                                                                 pos
                            case result of
                              Just x -> return $ x { tIsLvalue = True }
                              _      -> failTyping $ TypingError
                                (  "Struct doesn't have a field called `"
                                ++ s_unpack fieldName
                                ++ "`"
                                )
                                pos

                          Union { unionFields = fields } -> do
                            result <- typeStructUnionFieldAccess ctx
                                                                 tctx
                                                                 mod
                                                                 fields
                                                                 r1
                                                                 fieldName
                                                                 pos
                            case result of
                              Just x -> return $ x { tIsLvalue = True }
                              _      -> failTyping $ TypingError
                                (  "Union doesn't have a field called `"
                                ++ s_unpack fieldName
                                ++ "`"
                                )
                                pos

                          Abstract { abstractUnderlyingType = u } ->
                            -- forward to parent
                            typeFieldAccess u fieldName

              TypePtr x ->
                -- try to auto-dereference
                           r $ makeExprTyped
                (Field (makeExprTyped (PreUnop Deref r1) x (tPos r1))
                       (Var fieldName)
                )
                (inferredType ex)
                pos

              TypeTypeOf x@(mp, name) -> do
                -- look for a static method or field
                definitionMod <- getMod ctx mp
                subScope      <- getSubScope (modScope definitionMod) [name]
                findStatic    <- resolveLocal subScope fieldName
                case findStatic of
                  Just binding -> do
                    x <- typeVarBinding ctx fieldName binding pos
                    resolve $ TypeEq
                      (bindingConcrete binding)
                      (inferredType x)
                      "Field access must match the field's type"
                      (tPos x)
                    return x
                  Nothing -> failTyping $ TypingError
                    (  "Type "
                    ++ (s_unpack $ showTypePath x)
                    ++ " has no static field "
                    ++ s_unpack fieldName
                    )
                    pos

              TypeBox tp@(defMod, traitName) params -> do
                definitionMod <- getMod ctx defMod
                subScope <- getSubScope (modScope definitionMod) [traitName]
                trait         <- resolveLocal (modScope definitionMod) traitName
                traitDef      <- case trait of
                  Just (Binding { bindingType = TraitBinding t }) -> return t
                  _ -> throwk $ TypingError
                    ("Couldn't find trait: " ++ s_unpack (showTypePath tp))
                    pos
                method <- resolveLocal subScope fieldName
                case method of
                  Just binding -> do
                    x <- typeVarBinding ctx fieldName binding pos
                    resolve $ TypeEq
                      (bindingConcrete binding)
                      (inferredType x)
                      "Box field access must match the field's type"
                      (tPos x)
                    let
                      typed = makeExprTyped
                        (Field
                          (makeExprTyped (BoxedVtable traitDef r1)
                                         (inferredType r1)
                                         (tPos r1)
                          )
                          (Var fieldName)
                        )
                        (inferredType x)
                        (pos)
                    return $ typed
                      { tImplicits = (makeExprTyped
                                       (BoxedValue traitDef r1)
                                       (TypePtr $ TypeBasicType BasicTypeVoid)
                                       pos
                                     )
                        : tImplicits typed
                      }
                  Nothing -> failTyping $ TypingError
                    ((show t) ++ " has no field " ++ s_unpack fieldName)
                    pos

              x -> failTyping $ TypingError
                ("Field access is not allowed on " ++ show x)
                pos

                  -- TODO: if we have a module function called `fieldName` that
                  -- takes e1 as the first argument, fall back to that

        typeFieldAccess (inferredType r1) fieldName

    (Field e1 _) -> do
      throwk $ InternalError
        "Malformed AST: field access requires an identifier"
        (Just pos)

    (ArrayAccess e1 e2) -> do
      r1 <- r e1
      case (inferredType r1, tExpr e2) of
        (TypeTuple t, Literal (IntValue i _)) ->
          -- compile-time tuple slot access
          if (i >= 0) && (i < length t)
            then r (r1 {tExpr = TupleSlot r1 i, tPos = tPos r1 <+> tPos e2})
            else throwk $ TypingError ("Access to invalid tuple slot (tuple has " ++ show (length t) ++ " slots)") pos
        (TypeTuple t, _) ->
          throwk $ TypingError "Array access on tuples is only allowed using int literals" pos
        _ -> do
          r2 <- r e2
          throwk $ InternalError "Not yet implemented" (Just pos)

    (Cast e1 t) -> do
      r1 <- r e1
      tryRewrite (unknownTyped $ Cast r1 t) $ do
        let cast = return $ makeExprTyped (Cast r1 t) t pos
        case (inferredType r1, t) of
          (TypePtr (TypeBasicType BasicTypeVoid), TypePtr _) -> cast
          (x, y        ) -> do
            t' <- unify ctx tctx mod x y
            x' <- unify ctx tctx mod x (typeClassNumeric)
            y' <- unify ctx tctx mod y (typeClassNumeric)
            -- TODO: allow cast from abstract parent to child
            -- TODO: allow cast from value to box
            case (t', x', y') of
              (Just _, _     , _     ) -> cast
              (_     , Just _, Just _) -> cast
              _                        -> throwk $ TypingError
                ("Invalid cast: " ++ show x ++ " as " ++ show y)
                pos

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

    (VarDeclaration (Var vname) _ init) -> do
      let varType = inferredType ex
      init' <- case init of
        Just e1 -> do
          r1        <- r e1
          converted <- autoRefDeref ctx
                                    tctx
                                    mod
                                    varType
                                    (inferredType r1)
                                    r1
                                    []
                                    r1
          resolve $ TypeEq
            varType
            (inferredType converted)
            "A variable's initial value must match the variable's type"
            (tPos r1)
          return $ Just converted
        Nothing -> return Nothing
      existing <- resolveLocal (head $ tctxScopes tctx) vname
      case existing of
        Just (Binding { bindingPos = pos' }) ->
          throwk $ DuplicateDeclarationError (modPath mod) vname pos pos'
        _ -> bindToScope
          (head $ tctxScopes tctx)
          vname
          (newBinding
            ([], vname)
            (VarBinding
              (newVarDefinition { varName    = vname
                                , varType    = varType
                                , varDefault = init'
                                , varPos     = pos
                                }
              )
            )
            varType
            []
            pos
          )
      return $ makeExprTyped (VarDeclaration (Var vname) (varType) init')
                             varType
                             pos

    (Defer e1) -> do
      throwk $ InternalError "Not yet implemented" (Just pos)

    (StructInit structType@(TypeInstance tp@(mp, name) params) fields) -> do
      owningModule <- getMod ctx mp
      structDef    <- resolveLocal (modScope owningModule) name
      case structDef of
        Just (Binding { bindingType = TypeBinding (TypeDefinition { typeSubtype = Struct { structFields = structFields } }) })
          -> do
            let providedNames = map fst fields
            let fieldNames    = map varName structFields
            let extraNames    = providedNames \\ fieldNames
            -- TODO: check for duplicate fields
            -- check for extra fields
            unless (null extraNames) $ throwk $ BasicError
              (  "Struct "
              ++ s_unpack (showTypePath tp)
              ++ " has the following extra fields:\n\n"
              ++ intercalate
                   "\n"
                   [ "  - `" ++ s_unpack name ++ "`" | name <- extraNames ]
              ++ "\n\nRemove these fields or correct any typos."
              )
              (Just pos)
            let nonProvidedNames = fieldNames \\ providedNames
            typedFields <- forM
              (structFields)
              (\field -> do
                let fieldType = varType field
                let provided =
                      find (\(name, _) -> name == varName field) fields
                case provided of
                  Just (name, value) -> return ((name, value), fieldType)
                  Nothing            -> case varDefault field of
                    Just fieldDefault -> do
                      -- FIXME
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
                r1        <- r expr
                converted <- autoRefDeref ctx
                                          tctx
                                          mod
                                          fieldType
                                          (inferredType r1)
                                          r1
                                          []
                                          r1
                resolve $ TypeEq
                  fieldType
                  (inferredType converted)
                  "Provided struct field values must match the declared struct field type"
                  (tPos r1)
                return (name, converted)
              )
            return $ makeExprTyped (StructInit structType typedFields)
                                   structType
                                   pos

    (TupleInit slots) -> do
      slots' <- forM slots r
      return $ makeExprTyped (TupleInit slots')
                             (TypeTuple (map inferredType slots'))
                             pos

    _ -> return $ ex

  t' <- knownType ctx tctx mod (inferredType result)
  let result' = result { inferredType = t' }
  tryRewrite result' (return result')

alignCallArgs
  :: CompileContext
  -> TypeContext
  -> Module
  -> [ConcreteType]
  -> Bool
  -> [TypedExpr]
  -> [TypedExpr]
  -> IO [TypedExpr]
alignCallArgs ctx tctx mod argTypes isVariadic implicits args =
  if null argTypes
    then return []
    else do
      nextArg <- knownType ctx tctx mod (head argTypes)
      found   <- findImplicit ctx tctx mod nextArg implicits
      case found of
        Just x -> do
          rest <- alignCallArgs ctx
                                tctx
                                mod
                                (tail argTypes)
                                isVariadic
                                (delete x implicits)
                                args
          return $ x : rest
        Nothing -> return $ args

findImplicit
  :: CompileContext
  -> TypeContext
  -> Module
  -> ConcreteType
  -> [TypedExpr]
  -> IO (Maybe TypedExpr)
findImplicit ctx tctx mod ct []      = return Nothing
findImplicit ctx tctx mod ct (h : t) = do
  converted <- autoRefDeref ctx tctx mod ct (inferredType h) h [] h
  match     <- unify ctx tctx mod ct (inferredType converted)
  case match of
    Just _  -> return $ Just converted
    Nothing -> findImplicit ctx tctx mod ct t

typeFunctionCall
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypedExpr
  -> [TypedExpr]
  -> [TypedExpr]
  -> IO TypedExpr
typeFunctionCall ctx tctx mod e@(TypedExpr { inferredType = TypeFunction rt argTypes isVariadic, tPos = pos }) implicits args
  = do
    aligned <- alignCallArgs ctx
                             tctx
                             mod
                             (map snd argTypes)
                             isVariadic
                             implicits
                             args
    when
        (if isVariadic
          then length aligned < length argTypes
          else length aligned /= length argTypes
        )
      $ throwk
      $ TypingError
          (  "Expected "
          ++ (show $ length argTypes)
          ++ (if isVariadic then " or more" else "")
          ++ " arguments (called with "
          ++ (show $ length args)
          ++ " arguments and "
          ++ (show $ length implicits)
          ++ " implicits)"
          )
          pos
    converted <- forM
      (zip (map Just argTypes ++ repeat Nothing) aligned)
      (\(arg, argValue) -> case arg of
        Just (_, argType) -> autoRefDeref ctx
                                          tctx
                                          mod
                                          argType
                                          (inferredType argValue)
                                          argValue
                                          []
                                          argValue
        Nothing -> return argValue
      )
    forM_
      (zip argTypes converted)
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
    return $ makeExprTyped (Call e converted) rt pos

typeEnumConstructorCall
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypedExpr
  -> [TypedExpr]
  -> TypePath
  -> Str
  -> ConcreteArgs
  -> IO TypedExpr
typeEnumConstructorCall ctx tctx mod e args tp discriminant argTypes = do
  when (length args < length argTypes) $ throwk $ BasicError
    (  "Expected "
    ++ (show $ length argTypes)
    ++ " arguments (called with "
    ++ (show $ length args)
    ++ ")"
    )
    (Just $ tPos e)
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
                "Enum arg types must match the enum's declaration"
                (tPos argValue)
        )
    )
                                                                  -- TODO: params
  return $ makeExprTyped (EnumInit (TypeInstance tp []) discriminant args)
                         (TypeInstance tp [])
                         (tPos e)

typeStructUnionFieldAccess
  :: CompileContext
  -> TypeContext
  -> Module
  -> [VarDefinition TypedExpr ConcreteType]
  -> TypedExpr
  -> Str
  -> Span
  -> IO (Maybe TypedExpr)
typeStructUnionFieldAccess ctx tctx mod fields r fieldName pos = do
  case findStructUnionField fields fieldName of
    Just field -> do
      return $ Just $ makeExprTyped (Field r (Var fieldName))
                                    (varType field)
                                    pos
    Nothing -> return Nothing

findStructUnionField :: [VarDefinition a b] -> Str -> Maybe (VarDefinition a b)
findStructUnionField (h : t) fieldName =
  if varName h == fieldName then Just h else findStructUnionField t fieldName
findStructUnionField [] _ = Nothing

typeVarBinding :: CompileContext -> Str -> Binding -> Span -> IO TypedExpr
typeVarBinding ctx name binding pos = do
  let namespace = bindingNamespace binding
  let t         = bindingConcrete binding
  let tp        = bindingPath binding
  case bindingType binding of
    VarBinding _ ->
      return $ makeExprTyped (Identifier (Var name) namespace) t pos
    FunctionBinding _ ->
      return $ makeExprTyped (Identifier (Var name) namespace) t pos
    EnumConstructor (EnumVariant { variantName = discriminant, variantArgs = args })
      -> do -- TODO: handle type params
        let (TypeEnumConstructor tp _ _) = t
        return $ if null args
          then makeExprTyped (EnumInit (TypeInstance tp []) discriminant [])
                             (TypeInstance tp [])
                             pos
          else makeExprTyped
            (Identifier (Var name) [])
            (TypeEnumConstructor tp
                                 discriminant
                                 [ (argName arg, argType arg) | arg <- args ]
            )
            pos
    TypeBinding _ -> return
      $ makeExprTyped (Identifier (Var name) namespace) (TypeTypeOf tp) pos
    -- TODO: in instance method context, `this`
    -- TODO: in instance method context, all methods
    -- TODO: in struct/union instance method context, field names
    -- TODO: in any method context, `Self`
    -- TODO: in any method context, static methods
    -- TODO: in any method context, static fields

literalConstraints :: ValueLiteral b -> ConcreteType -> Span -> [TypeConstraint]
literalConstraints (BoolValue _) s pos =
  [TypeEq (basicType $ BasicTypeBool) s "Bool literal must be a Bool type" pos]
literalConstraints (IntValue v _) s pos =
  let exps = if v < 0
        then [-63, -53, -31, -24, -15, -7, 0] :: [Int]
        else [0, 7, 8, 15, 16, 24, 31, 32, 53, 63, 64] :: [Int]
  in
    let
      t = foldr
        (\(low, high) acc ->
          if v
               >= (signum low)
               *  (2 ^ abs low)
               && v
               <  (signum high)
               *  (2 ^ abs high)
            then typeClassRange (if v < 0 then low else high)
            else acc
        )
        typeClassNumeric
        (zip exps (drop 1 exps))
    in  [TypeEq t s "Int literals must be a Numeric type" pos]
literalConstraints (FloatValue _ _) s pos =
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
