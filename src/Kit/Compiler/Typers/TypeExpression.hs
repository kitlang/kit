module Kit.Compiler.Typers.TypeExpression where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.DumpAst
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TermRewrite
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.AutoRefDeref
import Kit.Compiler.Typers.Base
import Kit.Compiler.Typers.ConvertExpr
import Kit.Compiler.Typers.TypeLiteral
import Kit.Compiler.Typers.TypeOp
import Kit.Compiler.Unify
import Kit.Compiler.Utils
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
  let maybeR x = case x of
        Just x -> do
          x' <- r x
          return $ Just x'
        Nothing -> return Nothing
  let resolve constraint = resolveConstraint ctx tctx constraint
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
          addUsing ctx c using'
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
        Just (TypeInstance tp params) ->
          return $ makeExprTyped Self (TypeTypeOf tp) pos
        Nothing ->
          failTyping $ TypingError ("`Self` can only be used in methods") pos

    (Identifier v namespace) -> do
      case (tctxState tctx, v) of
        (TypingPattern, Var vname) -> do
          binding <- resolveVar ctx (tctxScopes tctx) mod vname
          case binding of
            Just binding@(Binding { bindingType = EnumConstructor _ }) -> do
              x <- typeVarBinding ctx tctx vname binding pos
              return $ x { tIsLvalue = True }
            _ -> return ex
        (_, Var vname) -> do
          tryRewrite
            (unknownTyped $ Identifier v namespace)
            (do
              binding <- resolveVar ctx (tctxScopes tctx) mod vname
              case binding of
                Just binding -> do
                  x <- typeVarBinding ctx tctx vname binding pos
                  return $ x { tIsLvalue = True }
                Nothing -> failTyping
                  $ TypingError ("Unknown identifier: " ++ s_unpack vname) pos
            )
        (_, MacroVar vname t) -> do
          tryRewrite
            (unknownTyped $ Identifier v namespace)
            (do
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

                  let boundSlots = filter
                        (\(i, (a, b)) -> tExpr a /= Identifier Hole [])
                        (zip [0 ..] (zip t t2))
                  slots <- forM boundSlots $ \(i, (a, b)) -> do
                    let
                      e1 = makeExprTyped
                        (Binop Assign
                               a
                               (makeExprTyped (TupleSlot tupleExpr i) b pos)
                        )
                        b
                        (tPos a)
                    case tExpr a of
                      Identifier _ _ -> return e1
                      _              -> r e1

                  return $ (makeExprTyped (Block slots) (inferredType r2) pos)
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
        lMixed <- unify ctx tctx (inferredType r1) (typeClassNumericMixed)
        rMixed <- unify ctx tctx (inferredType r2) (typeClassNumericMixed)
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
        (For (makeExprTyped (Identifier v []) tv pos1) r2 r3)
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
        TypeFunction _ _ _ _ ->
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

    (Match e1 cases (e2)) -> do
      r1 <- r e1
      r2 <- maybeR e2
      let tctx' = tctx { tctxState = TypingPattern }
      cases' <- forM cases $ \c -> do
        let tctx' = tctx { tctxState = TypingPattern }
        pattern <- typeExpr ctx tctx' mod $ matchPattern c
        resolve $ TypeEq
          (inferredType r1)
          (inferredType pattern)
          "Match pattern must match the type of the matched value"
          (tPos pattern)
        patternScope <- newScope []
        let ids = exprMapReduce
              (\x -> case tExpr x of
                Identifier (Var v) [] -> Just (v, inferredType x, tPos x)
                _                     -> Nothing
              )
              (\x acc -> case x of
                Just x  -> x : acc
                Nothing -> acc
              )
              tExpr
              []
              pattern
        forM_ ids $ \(id, t, pos) -> do
          bindToScope patternScope id $ newBinding
            ([], id)
            (VarBinding (newVarDefinition { varName = id, varType = t }))
            t
            []
            pos
        let tctx' = tctx { tctxScopes = patternScope : (tctxScopes tctx) }
        body <- typeExpr ctx tctx' mod $ matchBody c
        return $ MatchCase {matchPattern = pattern, matchBody = body}
      return $ makeExprTyped (Match r1 cases' r2) (voidType) pos

    (InlineCall e1) -> throwk $ InternalError "Not yet implemented" (Just pos)

    (Field e1 (Var fieldName)) -> do
      r1 <- r e1
      tryRewrite (unknownTyped $ Field r1 (Var fieldName)) $ do
        let
          typeFieldAccess t fieldName = do
            t <- follow ctx tctx t
            case t of
              TypeInstance tp@(modPath, typeName) params -> do
                templateDef <- getTypeDefinition ctx modPath typeName
                let tctx' = addTypeParams
                      tctx
                      [ (paramName param, val)
                      | (param, val) <- zip (typeParams templateDef) params
                      ]
                def <- followType ctx tctx' templateDef
                let subtype = typeSubtype def
                localScope <- getSubScope (modScope mod) [typeName]
                binding    <- resolveLocal localScope fieldName
                case binding of
                  Just x -> do
                    -- this is a local method
                    typed <- typeVarBinding ctx tctx' fieldName x pos
                    -- this may be a template; replace `this` with the actual
                    -- type to guarantee the implicit pass will work
                    let
                      f = case inferredType typed of
                        TypeFunction rt args varargs _ -> TypeFunction
                          rt
                          ( (let (name, _) = (head args)
                             in  (name, inferredType r1)
                            )
                          : (tail args)
                          )
                          varargs
                          params
                    return $ (makeExprTyped (Method r1 tp fieldName) f pos)
                      { tImplicits = r1 : tImplicits typed
                      }
                  _ -> case subtype of
                    Struct { structFields = fields } -> do
                      result <- typeStructUnionFieldAccess ctx
                                                           tctx'
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
                                                           tctx'
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
                    x <- typeVarBinding ctx tctx fieldName binding pos
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
                    x <- typeVarBinding ctx tctx fieldName binding pos
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
          then r
            (r1 { tExpr        = TupleSlot r1 i
                , inferredType = t !! i
                , tPos         = tPos r1 <+> tPos e2
                }
            )
          else throwk $ TypingError
            (  "Access to invalid tuple slot (tuple has "
            ++ show (length t)
            ++ " slots)"
            )
            pos
        (TypeTuple t, _) -> throwk $ TypingError
          "Array access on tuples is only allowed using int literals"
          pos
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
            t' <- unify ctx tctx x y
            x' <- unify ctx tctx x (typeClassNumeric)
            y' <- unify ctx tctx y (typeClassNumeric)
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
          converted <- autoRefDeref ctx tctx varType (inferredType r1) r1 [] r1
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

    (StructInit structType@(TypeInstance tp@(mp, name) p) fields) -> do
      params       <- makeGeneric ctx tp pos p
      owningModule <- getMod ctx mp
      structDef    <- getTypeDefinition ctx mp name
      let tctx' = addTypeParams tctx params
      case typeSubtype structDef of
        Struct { structFields = structFields } -> do
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
              fieldType <- follow ctx tctx' $ varType field
              let provided = find (\(name, _) -> name == varName field) fields
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
          let structType' = TypeInstance tp $ map snd params
          return $ makeExprTyped (StructInit structType' typedFields)
                                 structType'
                                 pos

    (TupleInit slots) -> do
      slots' <- forM slots r
      return $ makeExprTyped (TupleInit slots')
                             (TypeTuple (map inferredType slots'))
                             pos

    _ -> return $ ex

  t' <- follow ctx tctx (inferredType result)
  let result' = result { inferredType = t' }
  tryRewrite result' (return result')

alignCallArgs
  :: CompileContext
  -> TypeContext
  -> [ConcreteType]
  -> Bool
  -> [TypedExpr]
  -> [TypedExpr]
  -> IO [TypedExpr]
alignCallArgs ctx tctx argTypes isVariadic implicits args = if null argTypes
  then return []
  else do
    nextArg <- follow ctx tctx (head argTypes)
    found   <- findImplicit ctx tctx nextArg implicits
    case found of
      Just x -> do
        rest <- alignCallArgs ctx
                              tctx
                              (tail argTypes)
                              isVariadic
                              (delete x implicits)
                              args
        return $ x : rest
      Nothing -> return $ args

findImplicit
  :: CompileContext
  -> TypeContext
  -> ConcreteType
  -> [TypedExpr]
  -> IO (Maybe TypedExpr)
findImplicit ctx tctx ct []      = return Nothing
findImplicit ctx tctx ct (h : t) = do
  converted <- autoRefDeref ctx tctx ct (inferredType h) h [] h
  match     <- unify ctx tctx ct (inferredType converted)
  case match of
    Just _  -> return $ Just converted
    Nothing -> findImplicit ctx tctx ct t

typeFunctionCall
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypedExpr
  -> [TypedExpr]
  -> [TypedExpr]
  -> IO TypedExpr
typeFunctionCall ctx tctx mod e@(TypedExpr { inferredType = ft@(TypeFunction rt argTypes isVariadic params), tPos = pos }) implicits args
  = do
    aligned <- alignCallArgs ctx
                             tctx
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
          (  "Function expected "
          ++ (show $ length argTypes)
          ++ (if isVariadic then " or more" else "")
          ++ " argument"
          ++ (plural $ length argTypes)
          ++ ":\n\n  "
          ++ show ft
          ++ "\n\nCalled with "
          ++ (show $ length args)
          ++ " argument"
          ++ (plural $ length args)
          ++ (if null implicits
              then
                "."
              else
                (  ", and "
                ++ (show $ length implicits)
                ++ " implicit"
                ++ (plural $ length implicits)
                ++ ":\n\n"
                ++ intercalate
                     "\n"
                     [ "  - implicit " ++ (show $ inferredType i)
                     | i <- implicits
                     ]
                )
             )
          )
          pos
    converted <- forM
      (zip (map Just argTypes ++ repeat Nothing) aligned)
      (\(arg, argValue) -> case arg of
        Just (_, argType) -> autoRefDeref ctx
                                          tctx
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
        t1 <- follow ctx tctx argType
        t2 <- follow ctx tctx (inferredType argValue)
        resolveConstraint
          ctx
          tctx
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
  params <- makeGeneric ctx tp (tPos e) []
  forM_
    (zip argTypes args)
    (\((_, argType), argValue) -> do
      t1 <- follow ctx tctx argType
      t2 <- follow ctx tctx (inferredType argValue)
      resolveConstraint
        ctx
        tctx
        (TypeEq t1
                t2
                "Enum arg types must match the enum's declaration"
                (tPos argValue)
        )
    )
  let ct = TypeInstance tp (map snd params)
  return $ makeExprTyped (EnumInit ct discriminant args)
                         ct
                         (tPos e)

typeStructUnionFieldAccess
  :: CompileContext
  -> TypeContext
  -> [VarDefinition TypedExpr ConcreteType]
  -> TypedExpr
  -> Str
  -> Span
  -> IO (Maybe TypedExpr)
typeStructUnionFieldAccess ctx tctx fields r fieldName pos = do
  case findStructUnionField fields fieldName of
    Just field -> do
      return
        $ (Just $ (makeExprTyped (Field r (Var fieldName)) (varType field) pos) { tIsLvalue = True
                                                                                }
          )

    Nothing -> return Nothing

findStructUnionField :: [VarDefinition a b] -> Str -> Maybe (VarDefinition a b)
findStructUnionField (h : t) fieldName =
  if varName h == fieldName then Just h else findStructUnionField t fieldName
findStructUnionField [] _ = Nothing

typeVarBinding
  :: CompileContext -> TypeContext -> Str -> Binding -> Span -> IO TypedExpr
typeVarBinding ctx tctx name binding pos = do
  let namespace = bindingNamespace binding
  let t         = bindingConcrete binding
  let tp        = bindingPath binding
  case bindingType binding of
    VarBinding _ ->
      return $ makeExprTyped (Identifier (Var name) namespace) t pos
    FunctionBinding def -> if null $ functionParams def
      then return $ makeExprTyped (Identifier (Var name) namespace) t pos
      else do
-- TODO: allow specifying explicit function params
        params <- makeGeneric ctx tp pos []
        t'     <- mapType (follow ctx tctx) t
        t''    <- mapType (substituteParams params) t'
        let ft = case t'' of
              TypeFunction rt args varargs _ ->
                TypeFunction rt args varargs (map snd params)
        return $ makeExprTyped (Identifier (Var name) namespace) ft pos
    EnumConstructor (EnumVariant { variantName = discriminant, variantArgs = args })
      -> do -- TODO: handle type params
        let (TypeEnumConstructor tp@(modPath, typeName) _ _) = t
        def    <- getTypeDefinition ctx modPath typeName
        params <- makeGeneric ctx tp pos []
        let ct = TypeInstance tp (map snd params)
        return $ if null args
          then makeExprTyped (EnumInit ct discriminant []) ct pos
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
