module Kit.Compiler.Typers.TypeExpression where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TermRewrite
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.AutoRefDeref
import Kit.Compiler.Typers.TypeLiteral
import Kit.Compiler.Typers.TypeOp
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
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

-- partialTyping ex et e = return $ ex { tExpr = et, tError = Just $ KitError e }

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
  veryNoisyDebugLog ctx $ show ex
  let r = typeExpr ctx tctx mod
  let maybeR x = case x of
        Just x -> do
          x' <- r x
          return $ Just x'
        Nothing -> return Nothing
  let resolve constraint = resolveConstraint ctx tctx constraint
  let unknownTyped x = makeExprTyped x (TypeBasicType BasicTypeUnknown) pos
  let
    tryRewrite x y = do
      ownRules <- ownRules ctx tctx x
      let implicitRules =
            nub
              $  ownRules
              ++ (foldr (++) [] $ map tImplicitRules $ exprChildren $ tExpr x)
      result <- foldM
        (\acc rule -> do
          let thisType = case ruleThis rule of
                Just x  -> inferredType x
                Nothing -> TypeBasicType BasicTypeUnknown
          tctx <- genericTctx ctx tctx (tPos x) thisType
          case acc of
            Just x  -> return $ Just x
            Nothing -> do
              result <- rewriteExpr ctx
                                    tctx
                                    mod
                                    rule
                                    x
                                    (\tctx -> typeExpr ctx tctx mod)
              case result of
                Just x  -> return $ Just (x, tctx)
                Nothing -> return Nothing
        )
        Nothing
        (  implicitRules
        ++ [ rule | ruleset <- tctxRules tctx, rule <- ruleSetRules ruleset ]
        )
      case result of
        Just (x, tctx) -> r x
        Nothing        -> y

  result <- case et of
    (Block children) -> do
      blockScope <- newScope
      tctx <- return $ tctx { tctxScopes = blockScope : tctxScopes tctx }
      typedChildren <- forMWithErrors children $ typeExpr ctx tctx mod
      return $ makeExprTyped (Block typedChildren)
        --(if children == [] then TypeVoid else inferredType $ last typedChildren)
                                                   (TypeVoid) pos

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

    (Literal l _) -> do
      mapM_ resolve $ literalConstraints l (inferredType ex) pos
      return $ ex { tIsCompileTime = True }

    (This) -> do
      case tctxThis tctx of
        Just t -> return $ (makeExprTyped This t pos) { tIsLvalue = True }
        Nothing ->
          throwk $ TypingError ("`this` can only be used in methods") pos

    (Self) -> do
      case tctxSelf tctx of
        Just (TypeInstance tp params) ->
          return $ makeExprTyped Self (TypeTypeOf tp params) pos
        Just x -> throwk $ TypingError
          ("`Self` value of " ++ show x ++ " not yet supported")
          pos
        Nothing ->
          throwk $ TypingError ("`Self` can only be used in methods") pos

    (Identifier v) -> do
      case (tctxState tctx, v) of
        (TypingPattern, Var vname) -> do
          binding <- resolveVar ctx (tctxScopes tctx) mod (tpName vname)
          case binding of
            Just binding@(EnumConstructor _) -> do
              x <- typeVarBinding ctx tctx binding pos
              return $ x
            _ -> return ex
        (TypingExprOrType, Var vname) -> do
          x <-
            (try (typeExpr ctx (tctx { tctxState = TypingExpression }) mod ex)) :: IO
              (Either KitError TypedExpr)
          case x of
            Right x   -> return x
            Left  err -> return ex
        (TypingPattern, Hole) -> do
          return ex
        (_, Var vname) -> do
          tryRewrite
            ex
            (do
              binding <- case fst vname of
                [] -> resolveVar ctx (tctxScopes tctx) mod (tpName vname)
                _  -> return Nothing
              binding <- case binding of
                Just _ -> return binding
                _      -> lookupBinding ctx vname
              case binding of
                Just binding -> do
                  x <- typeVarBinding ctx tctx binding pos
                  return $ case inferredType x of
                    TypeFunction _ _ _ _ -> x { tIsLvalue = True }
                    _                    -> x
                Nothing ->
                  case
                      foldr
                        (\(paramName, paramType) acc ->
                          acc <|> case paramType of
                            ConstantType v | tpName paramName == tpName vname -> Just v
                            _              -> Nothing
                        )
                        Nothing
                        (tctxTypeParams tctx)
                    of
                      Just v -> do
                        tv <- makeTypeVar ctx pos
                        r $ makeExprTyped (Literal v tv) tv pos
                      Nothing -> throwk $ TypingError
                        (  "Unknown identifier: "
                        ++ (s_unpack $ showTypePath vname)
                        )
                        pos
            )
        (_, MacroVar vname t) -> do
          case find (\(name, _) -> name == vname) (tctxMacroVars tctx) of
            Just (name, expr) ->
              typeExpr
                  ctx
                  (tctx
                    { tctxMacroVars = delete (name, expr) (tctxMacroVars tctx)
                    }
                  )
                  mod
                $ expr
            Nothing -> return ex

    (TypeAnnotation e1 t) -> do
      r1 <- r e1
      let e = r1 { inferredType = t }
      tryRewrite e $ do
        resolve $ TypeEq
          t
          (inferredType r1)
          "Annotated expressions must match their type annotation"
          (tPos r1)
        return e

    (PreUnop Ref e1@(TypedExpr { tExpr = This })) -> do
      -- referencing `this` gives us a pointer that's also an lvalue
      r1 <- r e1
      return $ (makeExprTyped (PreUnop Ref r1) (TypePtr $ inferredType r1) pos)
        { tIsLvalue   = True
        , tIsLocalPtr = tIsLocal r1
        }

    (PreUnop op e1) -> do
      r1 <- r e1
      tryRewrite (unknownTyped $ PreUnop op r1) $ do
        case unopTypes op (inferredType r1) (inferredType ex) pos of
          Just constraints -> mapM_ resolve constraints
          Nothing          -> return () -- TODO
        return $ (makeExprTyped (PreUnop op r1) (inferredType ex) pos)
          { tIsLocalPtr = (op == Ref) && (tIsLocal r1)
          }

    (PostUnop op e1) -> do
      r1 <- r e1
      tryRewrite (unknownTyped $ PostUnop op r1) $ do
        case unopTypes op (inferredType r1) (inferredType ex) pos of
          Just constraints -> mapM_ resolve constraints
          Nothing          -> return () -- TODO
        return $ makeExprTyped (PostUnop op r1) (inferredType ex) pos

    (Binop Assign e1 e2) -> do
      r2 <- r e2
      tryRewrite (makeExprTyped (Binop Assign e1 r2) (inferredType ex) pos) $ do
        case tExpr e1 of
          TupleInit t -> do
            case inferredType r2 of
              TypeTuple t2 -> if length t == length t2
                then do
                  forMWithErrors_ (zip t t2) $ \(a, b) -> do
                    resolve $ TypeEq
                      (inferredType a)
                      b
                      "Tuple contents must match variables in tuple assignment"
                      pos

                  let
                    tupleExpr = if tIsLvalue r2
                      then r2
                      else (makeExprTyped (Temp r2) (inferredType r2) (tPos r2)) { tIsLocal = True
                                                                                 }

                  let boundSlots = filter
                        (\(i, (a, b)) -> tExpr a /= Identifier Hole)
                        (zip [0 ..] (zip t t2))
                  slots <- forMWithErrors boundSlots $ \(i, (a, b)) -> do
                    let
                      e1 = makeExprTyped
                        (Binop Assign
                               a
                               (makeExprTyped (TupleSlot tupleExpr i) b pos)
                        )
                        b
                        (tPos a)
                    case tExpr a of
                      Identifier _ -> return e1
                      _            -> r e1

                  return $ (makeExprTyped (Block slots) (inferredType r2) pos)
                else throwk $ TypingError
                  ("Tuples can only be assigned to tuples of the same size; actual type: "
                  ++ show (inferredType r2)
                  )
                  pos
              _ -> throwk $ TypingError
                ("Tuples can only be assigned to matching tuples; actual type: "
                ++ show (inferredType r2)
                )
                pos
          _ -> do
            r1        <- r e1
            converted <- tryAutoRefDeref ctx tctx (inferredType r1) r2
            when (tIsConst r1) $ throwk $ TypingError
              "Can't reassign a const value"
              pos
            resolve $ TypeEq
              (inferredType r1)
              (inferredType converted)
              "Assigned value must match the type of the lvalue it's assigned to"
              (tPos converted)
            return $ makeExprTyped (Binop Assign r1 converted)
                                   (inferredType r1)
                                   pos
    (Binop (AssignOp op) e1 e2) | (op == And) || (op == Or) -> do
      r1 <- r e1
      r2 <- r e2
      tryRewrite (unknownTyped $ Binop (AssignOp op) r1 r2) $ do
        resolve $ TypeEq
          (inferredType r1)
          (inferredType r2)
          "Assigned value must match the type of the lvalue it's assigned to"
          (tPos r2)
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
      tryRewrite (unknownTyped $ Binop op r1 r2) $ throwk $ BasicError
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
        let isMixed x = case x of
              Just x -> not $ or
                [ case i of
                    TypeVarConstraint _ _ -> True
                    _                     -> False
                | i <- x
                ]
              Nothing -> False
        tv <- makeTypeVar ctx pos
        case
            binopTypes op
                       (inferredType r1)
                       (inferredType r2)
                       tv
                       (isMixed lMixed)
                       (isMixed rMixed)
                       pos
          of
            Just constraints -> mapM_ resolve constraints
            Nothing          -> return () -- TODO
        return $ makeExprTyped (Binop op r1 r2) tv pos

    (For e1@(TypedExpr { tExpr = Identifier (Var id) }) e2 e3) -> do
      r2 <- r e2
      tv <- follow ctx tctx $ inferredType e1
      case tExpr r2 of
        RangeLiteral eFrom eTo -> do
          forMWithErrors_ [eFrom, eTo] $ \x -> resolve $ TypeEq
            tv
            (inferredType x)
            "For identifier must match the iterator's type"
            (tPos x)

          scope <- newScope
          bindToScope scope (tpName id)
            $ VarBinding
                (newVarDefinition { varName    = id
                                  , varType    = tv
                                  , varIsConst = True
                                  }
                )
          r3 <- typeExpr
            ctx
            (tctx { tctxScopes    = scope : (tctxScopes tctx)
                  , tctxLoopCount = (tctxLoopCount tctx) + 1
                  }
            )
            mod
            e3

          return $ makeExprTyped (For (e1 { inferredType = tv }) r2 r3)
                                 TypeVoid
                                 pos

        _ -> do
          tryRewrite (makeExprTyped (For e1 r2 e3) TypeVoid pos) $ do
            let
              tryIterable = do
                -- try to convert to an Iterable
                let
                  fail = throwk $ TypingError
                    "For statements must iterate over a supported type, such as `Iterator` or `Iterable`, with a `for` rewrite rule"
                    pos
                box <- autoRefDeref ctx
                                    tctx
                                    (TypeBox typeClassIterablePath [])
                                    r2
                case box of
                  Just box -> do
                    box <- r box
                    tryRewrite (makeExprTyped (For e1 box e3) TypeVoid pos)
                      $ fail
                  Nothing -> fail
            -- try to convert to an Iterator
            box <- autoRefDeref ctx tctx (TypeBox typeClassIteratorPath [tv]) r2
            case box of
              Just box -> do
                box <- r box
                tryRewrite (makeExprTyped (For e1 box e3) TypeVoid pos)
                  $ tryIterable
              Nothing -> tryIterable

    (For e1 e2 e3) -> do
      r1 <- typeExpr ctx (tctx { tctxState = TypingPattern }) mod e1
      case tExpr r1 of
        Identifier (Var _) -> r $ makeExprTyped (For r1 e2 e3) TypeVoid pos
        _ -> throwk $ TypingError ("Invalid for loop iterable") (tPos r1)

    (While e1 e2 d) -> do
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
                       (TypeBool)
                       "A while condition must be a Bool"
                       (tPos r1)
      return $ makeExprTyped (While r1 r2 d) TypeVoid pos

    (If e1 e2 (Just e3)) -> do
      r1       <- r e1
      scope1   <- newScope
      [r2, r3] <- forMWithErrors [e2, e3] $ \e -> do
        scope <- newScope
        typeExpr ctx (tctx { tctxScopes = scope : (tctxScopes tctx) }) mod e
      tv <- makeTypeVar ctx pos
      resolve $ TypeEq (inferredType r1)
                       (TypeBool)
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
      scope <- newScope
      r2    <- typeExpr ctx
                        (tctx { tctxScopes = scope : (tctxScopes tctx) })
                        mod
                        e2
      resolve $ TypeEq (inferredType r1)
                       (TypeBool)
                       "An if condition must be a Bool"
                       (tPos r1)
      return $ makeExprTyped (If r1 r2 Nothing) TypeVoid pos

    (Continue) -> do
      if (tctxLoopCount tctx > 0)
        then return $ makeExprTyped (Continue) TypeVoid pos
        else throwk $ TypingError "Can't `continue` outside of a loop" pos

    (Break) -> do
      if (tctxLoopCount tctx > 0)
        then return $ makeExprTyped (Break) TypeVoid pos
        else throwk $ TypingError "Can't `break` outside of a loop" pos

    (Return e1) -> do
      r1 <- typeMaybeExpr ctx tctx mod e1
      case (tctxReturnType tctx, r1) of
        (Just rt, Just r1) -> do
          rt <- follow ctx tctx rt
          r1 <- tryAutoRefDeref ctx tctx rt r1
          resolve $ TypeEq (rt)
                           (inferredType r1)
                           "Return type should match function return type"
                           (tPos r1)
          -- make sure we aren't returning any pointers to the stack
          let
            localPointers = exprFilterMapReduce
              (\x -> case tExpr x of
                PreUnop Deref _ -> False
                Call _ _ _      -> False
                _               -> True
              )
              (\x -> if (isPtr $ inferredType x) && (tIsLocalPtr x)
                then [x]
                else []
              )
              (++)
              tExpr
              []
              r1
          unless (null localPointers) $ throwk $ KitErrors
            [ KitError
              $ TypingError "Can't return a pointer to a local value"
              $ tPos ptr
            | ptr <- localPointers
            ]

          return $ makeExprTyped (Return $ Just r1) TypeVoid pos
        (Just rt, Nothing) -> do
          resolve $ TypeEq TypeVoid
                           (rt)
                           "Empty return is only allowed in Void functions"
                           pos
          return $ makeExprTyped (Return Nothing) TypeVoid pos
        (Nothing, _) ->
          throwk $ TypingError "Can't `return` outside of a function" pos

    (Call e1 _ args) -> do
      tryRewrite (unknownTyped $ Call e1 [] args) $ do
        (r1 : typedArgs) <- mapM r $ e1 : args
        modImp           <- modImplicits mod
        let untypedImplicits = tImplicits r1 ++ tctxImplicits tctx ++ modImp
        implicits <- forM untypedImplicits $ \i -> do
          t <- mapType (follow ctx tctx) $ inferredType i
          return $ i { inferredType = t }
        tryRewrite (unknownTyped $ Call r1 [] typedArgs) $ do
          let
            fail t =
              throwk $ TypingError ("Type " ++ show t ++ " is not callable") pos
          let
            typeCall t = case t of
              TypePtr t@(TypeFunction _ _ _ _) ->
                -- function pointer call
                                                  typeFunctionCall
                ctx
                tctx
                mod
                (r1 { inferredType = t })
                implicits
                typedArgs
              TypeFunction _ _ _ _ -> typeFunctionCall
                ctx
                tctx
                mod
                (r1 { inferredType = t })
                implicits
                typedArgs
              TypeEnumConstructor tp discriminant argTypes params -> do
                typeEnumConstructorCall ctx
                                        tctx
                                        mod
                                        (r1 { inferredType = t })
                                        typedArgs
                                        tp
                                        discriminant
                                        argTypes
                                        params
              TypeInstance tp params -> do
                templateDef <- getTypeDefinition ctx tp
                let
                  tctx' =
                    (addTypeParams
                        tctx
                        [ (typeSubPath templateDef $ paramName param, val)
                        | (param, val) <- zip (typeParams templateDef) params
                        ]
                      )
                      { tctxSelf = Just t
                      }
                def <- followType ctx tctx' templateDef
                let subtype = typeSubtype def
                case subtype of
                  Abstract { abstractUnderlyingType = u } ->
                    -- forward to parent
                    typeCall u
                  _ -> fail t
              _ -> fail t

          typeCall $ inferredType r1

    (Match e1 cases (e2)) -> do
      r1 <- r e1
      r1 <- return $ if tIsLvalue r1 then r1 else makeLvalue r1
      r2 <- maybeR e2
      let tctx' = tctx { tctxState = TypingPattern }
      cases' <- forMWithErrors cases $ \c -> do
        let tctx' = tctx { tctxState = TypingPattern }
        pattern <- typeExpr ctx tctx' mod $ matchPattern c
        resolve $ TypeEq
          (inferredType r1)
          (inferredType pattern)
          "Match pattern must match the type of the matched value"
          (tPos pattern)
        patternScope <- newScope
        let ids = exprMapReduce
              (\x -> case tExpr x of
                Identifier (Var v) -> Just (v, inferredType x, tPos x)
                _                  -> Nothing
              )
              (\x acc -> case x of
                Just x  -> x : acc
                Nothing -> acc
              )
              tExpr
              []
              pattern
        forMWithErrors_ ids $ \(id, t, pos) -> do
          bindToScope patternScope (tpName id)
            $ VarBinding
                (newVarDefinition { varName    = id
                                  , varType    = t
                                  , varIsConst = True
                                  }
                )
        let tctx' = tctx { tctxScopes = patternScope : (tctxScopes tctx) }
        body <- typeExpr ctx tctx' mod $ matchBody c
        return $ MatchCase {matchPattern = pattern, matchBody = body}
      return $ makeExprTyped (Match r1 cases' r2) (TypeVoid) pos

    (InlineCall e1) -> throwk $ InternalError "Not yet implemented" (Just pos)

    (Field e1 (Var fieldName)) -> do
      r1 <- r e1
      tryRewrite (unknownTyped $ Field r1 (Var fieldName)) $ do
        let
          typeFieldAccess t fieldName = do
            t <- follow ctx tctx t
            let failNotPublic = throwk $ TypingError
                  (  "Can't access non-public field "
                  ++ s_unpack fieldName
                  ++ " of "
                  ++ show t
                  )
                  pos
            case t of
              TypePtr x ->
                -- try to auto-dereference
                           r $ makeExprTyped
                (Field (makeExprTyped (PreUnop Deref r1) x (tPos r1))
                       (Var ([], fieldName))
                )
                (inferredType ex)
                pos

              TypeTypeOf tp params -> do
                -- look for a static method or field
                findStatic <- lookupBinding ctx $ subPath tp fieldName
                case findStatic of
                  Just binding -> do
                    params <- makeGeneric ctx tp pos params
                    tctx   <- genericTctx ctx
                                          tctx
                                          pos
                                          (TypeTypeOf tp $ map snd params)
                    params <- forMWithErrors (map snd params) $ mapType $ follow
                      ctx
                      tctx
                    let tctx' =
                          tctx { tctxSelf = Just $ TypeInstance tp params }
                    let accessible = case tctxSelf tctx' of
                          Just (TypeInstance tp2 params2) | tp2 == tp -> True
                          _ -> bindingIsPublic binding
                    when (not accessible) failNotPublic
                    x <- typeVarBinding ctx tctx' binding pos
                    f <- mapType (follow ctx tctx') $ inferredType x
                    -- when calling an instance method statically, remove MethodTarget
                    f <- return $ case f of
                      TypeFunction rt ((name, MethodTarget t) : args) varargs params
                        -> TypeFunction rt ((name, t) : args) varargs params
                      _ -> f
                    case binding of
                      FunctionBinding _ -> return $ makeExprTyped
                        ((StaticMember tp params) fieldName)
                        f
                        pos
                      VarBinding _ -> return $ makeExprTyped
                        ((StaticMember tp params) fieldName)
                        f
                        pos
                      _ -> return $ x { inferredType = f }
                  Nothing -> throwk $ TypingError
                    (  "Type "
                    ++ (s_unpack $ showTypePath tp)
                    ++ " has no static field "
                    ++ s_unpack fieldName
                    )
                    pos

              TypeBox tp params -> do
                case tExpr r1 of
                  BoxedVtable _ _ -> return ex
                  _               -> do
                    trait    <- lookupBinding ctx tp
                    traitDef <- case trait of
                      Just (TraitBinding t) -> return t
                      _                     -> throwk $ TypingError
                        ("Couldn't find trait: " ++ s_unpack (showTypePath tp))
                        pos
                    let
                      tctx' =
                        (addTypeParams
                            tctx
                            [ (traitSubPath traitDef $ paramName param, val)
                            | (param, val) <- zip (traitAllParams traitDef) params
                            ]
                          )
                          { tctxSelf = Just t
                          }
                    method <- lookupBinding ctx $ subPath tp fieldName
                    case method of
                      Just binding -> do
                        x <- typeVarBinding ctx tctx' binding pos
                        let
                          typed = makeExprTyped
                            (Field
                              (makeExprTyped (BoxedVtable traitDef r1)
                                             (inferredType r1)
                                             (tPos r1)
                              )
                              (Var ([], fieldName))
                            )
                            (inferredType x)
                            (pos)
                        -- trait <- followTrait ctx tctx' (modPath mod) traitDef
                        t <- mapType (follow ctx tctx') (inferredType x)
                        return $ typed
                          { tImplicits   = [ makeExprTyped
                                               (BoxedValue r1)
                                               (MethodTarget $ TypePtr TypeVoid)
                                               pos
                                           ]
                          , inferredType = t
                          }
                      Nothing -> throwk $ TypingError
                        ((show t) ++ " has no field " ++ s_unpack fieldName)
                        pos

              TypeTraitConstraint (tp, params) -> do
                let ref = addRef r1
                trait    <- lookupBinding ctx tp
                traitDef <- case trait of
                  Just (TraitBinding t) -> return t
                  _                     -> throwk $ TypingError
                    ("Couldn't find trait: " ++ s_unpack (showTypePath tp))
                    pos
                methodBinding <- lookupBinding ctx (subPath tp fieldName)
                case methodBinding of
                  Just binding -> do
                    tctx <- genericTctx ctx
                                        tctx
                                        pos
                                        (TypeTraitConstraint (tp, params))
                    result <- typeVarBinding ctx tctx binding pos
                    t      <- mapType (follow ctx tctx) $ inferredType result
                    return
                      $ (makeExprTyped (Field r1 $ Var ([], fieldName)) t pos)
                          { tImplicits = if null $ tImplicits r1
                                         then
                                           [ ref
                                               { inferredType = ( MethodTarget
                                                                $ TypePtr
                                                                $ TypeVoid
                                                                )
                                               }
                                           ]
                                         else
                                           tImplicits r1
                          }
                  _ -> throwk $ TypingError
                    (  "Trait "
                    ++ s_unpack (showTypePath tp)
                    ++ " has no field "
                    ++ s_unpack fieldName
                    )
                    pos

              TypeInstance tp params -> do
                templateDef <- getTypeDefinition ctx tp
                let
                  tctx' =
                    (addTypeParams
                        tctx
                        [ (typeSubPath templateDef $ paramName param, val)
                        | (param, val) <- zip (typeParams templateDef) params
                        ]
                      )
                      { tctxSelf = Just t
                      }
                def <- followType ctx tctx' templateDef
                let subtype = typeSubtype def
                binding <- lookupBinding ctx $ subPath tp fieldName
                case binding of
                  Just x -> do
                    let accessible = case tctxSelf tctx of
                          Just (TypeInstance tp2 params2) | tp2 == tp -> True
                          _ -> bindingIsPublic x
                    when (not accessible) failNotPublic
                    -- this is a local method
                    typed' <- typeVarBinding ctx tctx' x pos
                    t      <- mapType (follow ctx tctx') (inferredType typed')
                    let typed = typed' { inferredType = t }
                    -- this may be a template; replace `this` with the actual
                    -- type to guarantee the implicit pass will work
                    let
                      f = case inferredType typed of
                        TypeFunction rt args varargs _ -> TypeFunction
                          rt
                          ((let (name, _) = (head args)
                            in  (name, MethodTarget $ TypePtr $ inferredType r1)
                           )
                          : (tail args)
                          )
                          varargs
                          params
                        _ -> throwk $ TypingError
                          "Static fields and methods can't be accessed from individual values"
                          pos
                    return
                      $ (makeExprTyped (StaticMember tp params fieldName) f pos)
                          { tImplicits = (r1
                                           { inferredType = MethodTarget
                                             (inferredType r1)
                                           }
                                         )
                            : tImplicits typed
                          }

                  _ -> case subtype of
                    Struct { structFields = fields } -> do
                      result <- typeStructUnionFieldAccess ctx
                                                           tctx'
                                                           t
                                                           fields
                                                           r1
                                                           fieldName
                                                           pos
                      case result of
                        Just x -> return $ x { tIsLvalue = True }
                        _      -> throwk $ TypingError
                          (  "Struct "
                          ++ s_unpack (showTypePath tp)
                          ++ " doesn't have a field called `"
                          ++ s_unpack fieldName
                          ++ "`"
                          )
                          pos

                    Union { unionFields = fields } -> do
                      result <- typeStructUnionFieldAccess ctx
                                                           tctx'
                                                           t
                                                           fields
                                                           r1
                                                           fieldName
                                                           pos
                      case result of
                        Just x -> return $ x { tIsLvalue = True }
                        _      -> throwk $ TypingError
                          (  "Union doesn't have a field called `"
                          ++ s_unpack fieldName
                          ++ "`"
                          )
                          pos

                    Abstract { abstractUnderlyingType = u } ->
                      -- forward to parent
                      typeFieldAccess u fieldName

                    x -> throwk $ TypingError
                      ("Field access is not allowed on " ++ show x)
                      pos

              TypeAnonStruct _ fields ->
                case
                    find
                      (\(structFieldName, _) -> structFieldName == fieldName)
                      fields
                  of
                    Just (fieldName, fieldType) -> do
                      resolve $ TypeEq
                        fieldType
                        (inferredType ex)
                        "Struct field access must match the field's type"
                        (tPos r1)
                      return $ makeExprTyped (Field r1 $ Var ([], fieldName))
                                             fieldType
                                             pos
                    Nothing -> throwk $ TypingError
                      (  show t
                      ++ " doesn't have a field called "
                      ++ s_unpack fieldName
                      )
                      pos

              TypeAnonUnion _ fields ->
                case
                    find (\(unionFieldName, _) -> unionFieldName == fieldName)
                         fields
                  of
                    Just (fieldName, fieldType) -> do
                      resolve $ TypeEq
                        fieldType
                        (inferredType ex)
                        "Union field access must match the field's type"
                        (tPos r1)
                      return $ makeExprTyped (Field r1 $ Var ([], fieldName))
                                             fieldType
                                             pos
                    Nothing -> throwk $ TypingError
                      (  show t
                      ++ " doesn't have a field called "
                      ++ s_unpack fieldName
                      )
                      pos

              ModuleType tp -> do
                let newPath = subPath tp fieldName
                binding <- lookupBinding ctx newPath
                case binding of
                  Just binding -> do
                    x <- typeVarBinding ctx tctx binding pos
                    return $ x { tIsLvalue = True }
                  Nothing -> throwk $ TypingError
                    ("Unknown identifier " ++ s_unpack (showTypePath newPath))
                    pos

              x -> throwk $ TypingError
                ("Field access is not allowed on " ++ show x)
                pos

        result <-
          (try $ typeFieldAccess (inferredType r1) (tpName fieldName)) :: IO
            (Either KitError TypedExpr)
        case result of
          Right r   -> return r
          Left  err -> do
            let fail = do
                  case inferredType r1 of
                    TypePtr x ->
                      -- try to auto-dereference
                                 r $ makeExprTyped
                      (Field (makeExprTyped (PreUnop Deref r1) x (tPos r1))
                             (Var ([], tpName fieldName))
                      )
                      (inferredType ex)
                      pos
                    _ -> throw err
            -- UFCS: check for a function that takes the LHS as its first argument
            binding <- resolveVar ctx (tctxScopes tctx) mod (tpName fieldName)
            let
              fn tp ct =
                ((makeExprTyped (Identifier (Var tp)) ct pos)
                  { tImplicits = [ r1
                                     { inferredType = ( MethodTarget
                                                      $ inferredType r1
                                                      )
                                     }
                                 ]
                  }
                )
            case binding of
              Just (VarBinding v@(VarDefinition { varType = ct@(TypeFunction _ _ _ _) }))
                -> return $ (fn (varName v) ct) { tIsConst = varIsConst v }
              Just (FunctionBinding f) -> do
                return $ fn (functionName f) $ case functionConcrete f of
                  TypeFunction rt ((name, t) : args) varargs params ->
                    TypeFunction rt
                                 ((name, MethodTarget t) : args)
                                 varargs
                                 params
                  t -> t
              Just (TraitBinding t) -> do
                -- static trait dispatch: get a trait implementation for this type
                -- FIXME: this doesn't work for generic traits
                let
                  checkForImpl ex = do
                    let (forType, isStatic) = case inferredType ex of
                          TypeTypeOf tp params ->
                            (TypeInstance tp params, True)
                          t -> (t, False)
                    impl <- getTraitImpl ctx tctx (traitName t, []) forType
                    case impl of
                      Just x -> do
                        return
                          $ (makeExprTyped
                              (StaticVtable x)
                              (TypeTraitConstraint (traitName t, []))
                              pos
                            )
                              { tImplicits = if isStatic
                                             then
                                               []
                                             else
                                               [ (addRef ex)
                                                   { inferredType = (MethodTarget
                                                                    $ TypePtr
                                                                        TypeVoid
                                                                    )
                                                   }
                                               ]
                              , tIsLvalue  = True
                              }
                      Nothing -> case addDeref ex of
                        Just ex -> checkForImpl ex
                        Nothing -> throwk $ TypingError
                          (  "Couldn't find an implementation of trait "
                          ++ s_unpack (showTypePath $ traitName t)
                          ++ " for type "
                          ++ show (inferredType r1)
                          )
                          pos
                checkForImpl r1
              _ -> fail

    (Field e1 _) -> do
      throwk $ InternalError
        "Malformed AST: field access requires an identifier"
        (Just pos)

    (ArrayAccess e1 e2) -> do
      r1 <- r e1
      r2 <- typeExpr ctx (tctx { tctxState = TypingExprOrType }) mod e2
      tryRewrite (makeExprTyped (ArrayAccess r1 r2) (inferredType ex) pos) $ do
        let fail = throwk $ TypingError
              (  "Array access is not supported on values of type "
              ++ show (inferredType r1)
              )
              pos
        let
          resolveArrayAccess t = case (t, tExpr r2) of
            (TypeTuple t, Literal (IntValue i) _) ->
              -- FIXME: this should work with any constant Int expression
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

            (TypeArray t _, _) -> do
              resolveConstraint
                ctx
                tctx
                (TypeEq t
                        (inferredType ex)
                        "Array access on an array will return the inner type"
                        (tPos ex)
                )
              resolveConstraint
                ctx
                tctx
                (TypeEq
                  typeClassIntegral
                  (inferredType r2)
                  "Array access on an array requires an Integral argument"
                  (tPos r2)
                )
              return $ (makeExprTyped (ArrayAccess r1 r2) t pos) { tIsLvalue = True
                                                                 }

            (TypePtr t, _) -> do
              resolveConstraint
                ctx
                tctx
                (TypeEq
                  t
                  (inferredType ex)
                  "Array access on a pointer will dereference the pointer"
                  (tPos ex)
                )
              resolveConstraint
                ctx
                tctx
                (TypeEq
                  typeClassIntegral
                  (inferredType r2)
                  "Array access on a pointer requires an Integral argument"
                  (tPos r2)
                )
              return $ makeExprTyped (ArrayAccess r1 r2) (inferredType ex) pos

            (TypeInstance tp params, _) -> do
              def <- getTypeDefinition ctx tp
              case typeSubtype def of
                Abstract { abstractUnderlyingType = t } -> do
                  tctx <- genericTctx ctx tctx pos (TypeInstance tp params)
                  t    <- follow ctx tctx t
                  resolveArrayAccess t
                _ -> fail

            (TypeTypeOf tp params, _) -> do
              if tIsCompileTime r2
                then case tExpr r2 of
                  Literal v _ -> return $ r1
                    { inferredType = TypeTypeOf tp (params ++ [ConstantType v])
                    }
                  _ -> throwk $ TypingError
                    (  "Unknown constant expression used as constant type: "
                    ++ show r2
                    )
                    pos
                else do
                  t <- exprToType ctx tctx mod (tPos r2) r2
                  case t of
                    Just t -> return
                      $ r1 { inferredType = TypeTypeOf tp (params ++ [t]) }
                    Nothing -> throwk $ TypingError
                      (  "Unknown type parameter value: "
                      ++ show (inferredType r2)
                      )
                      pos

            (TypeTraitConstraint (tp, params), _) -> do
              if tIsCompileTime r2
                then case tExpr r2 of
                  Literal v _ -> return $ r1
                    { inferredType = TypeTraitConstraint
                      (tp, params ++ [ConstantType v])
                    }
                  _ -> throwk $ TypingError
                    (  "Unknown constant expression used as constant type: "
                    ++ show r2
                    )
                    pos
                else do
                  t <- exprToType ctx tctx mod (tPos r2) r2
                  case t of
                    Just t -> return $ r1
                      { inferredType = TypeTraitConstraint (tp, params ++ [t])
                      }
                    Nothing -> throwk $ TypingError
                      (  "Unknown type parameter value: "
                      ++ show (inferredType r2)
                      )
                      pos

            _ -> fail

        resolveArrayAccess $ inferredType r1

    (Cast e1 t) -> do
      t  <- mapType (follow ctx tctx) t
      t  <- makeGenericConcrete ctx pos t
      r1 <- r e1
      tryRewrite (makeExprTyped (Cast r1 t) t pos) $ do
        let cast = return $ makeExprTyped (Cast r1 t) t pos
        let invalidCast = throwk $ TypingError
              ("Invalid cast: " ++ show (inferredType r1) ++ " as " ++ show t)
              pos
        let
          typeCast rt = case (rt, t) of
            (TypeBox _ _, TypePtr (TypeBasicType BasicTypeVoid)) ->
              return $ makeExprTyped (BoxedValue r1) (TypePtr TypeVoid) pos
            (TypePtr _    , TypePtr (TypeBasicType BasicTypeVoid)) -> cast
            (TypeArray _ _, TypePtr (TypeBasicType BasicTypeVoid)) -> cast
            (TypeArray t _, TypePtr t2                           ) -> do
              t' <- unifyStrict ctx tctx t2 t
              case t' of
                Just _ -> cast
                _      -> invalidCast
            (TypePtr _, TypeBasicType BasicTypeCSize) -> cast
            (x        , y@(TypeBox tp params)       ) -> do
              box <- autoRefDeref ctx tctx y r1
              case box of
                Just box -> return box
                _        -> throwk $ TypingError
                  (  show x
                  ++ " can't be converted to a "
                  ++ show y
                  ++ "; no matching trait implementation found"
                  )
                  pos
            (x@(TypeInstance tp params), y) -> do
              t' <- unify ctx tctx x y
              case t' of
                Just x -> cast
                _      -> do
                  templateDef <- getTypeDefinition ctx tp
                  let
                    tctx' =
                      (addTypeParams
                          tctx
                          [ (typeSubPath templateDef $ paramName param, val)
                          | (param, val) <- zip (typeParams templateDef) params
                          ]
                        )
                        { tctxSelf = Just t
                        }
                  def <- followType ctx tctx' templateDef
                  let subtype = typeSubtype def
                  case subtype of
                    Abstract { abstractUnderlyingType = u } ->
                      -- forward to parent
                      typeCast u
                    _ -> invalidCast
            (x, y) -> do
              t' <- unify ctx tctx x y
              x' <- unify ctx tctx x (typeClassNumeric)
              y' <- unify ctx tctx y (typeClassNumeric)
              case (t', x', y') of
                (Just _, _     , _     ) -> cast -- FIXME
                (_     , Just _, Just _) -> cast
                _                        -> invalidCast
        typeCast $ inferredType r1

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

    (ArrayLiteral items) -> do
      items <- mapM r items
      case inferredType ex of
        TypeArray t s -> do
          when (s > 0) $ do
            unless (length items == s) $ throwk $ TypingError
              (  "The Array's length parameter ("
              ++ show s
              ++ ") must match the number of elements ("
              ++ show (length items)
              ++ ")"
              )
              pos
          forM_ items $ \val -> resolve $ TypeEq
            t
            (inferredType val)
            "Array elements must match the array's value type"
            (tPos val)
        _ -> throwk $ TypingError "Array literals must be typed as arrays" pos
      return $ makeExprTyped (ArrayLiteral items) (inferredType ex) pos

    (VarDeclaration s@(MacroVar vname _) a const b) -> do
      case find (\(name, _) -> name == vname) (tctxMacroVars tctx) of
        Just (_, x@TypedExpr { tExpr = Identifier v@(Var vname) }) ->
          r $ makeExprTyped (VarDeclaration v a const b) (inferredType ex) pos
        x -> throwk $ InternalError
          (  "Macro var $"
          ++ s_unpack vname
          ++ " isn't bound to an identifier: "
          ++ show x
          )
          (Just pos)

    (VarDeclaration (Var vname) _ const init) -> do
      when (const && isNothing init) $ throwk $ TypingError
        ("const must have an initial value")
        pos

      let
        typeVarDec = do
          varType <- makeGenericConcrete ctx pos $ inferredType ex
          varType <- follow ctx tctx varType
          init'   <- case init of
            Just e1 -> do
              r1        <- r e1
              converted <- tryAutoRefDeref ctx tctx varType r1
              resolve $ TypeEq
                varType
                (inferredType converted)
                "A variable's initial value must match the variable's type"
                (tPos r1)
              return $ Just converted
            Nothing -> return Nothing
          existing <- resolveLocal (head $ tctxScopes tctx) (tpName vname)
          case existing of
            Just x -> throwk $ DuplicateDeclarationError (modPath mod)
                                                         (tpName vname)
                                                         pos
                                                         (bindingPos x)
            _ -> bindToScope
              (head $ tctxScopes tctx)
              (tpName vname)
              (VarBinding
                (newVarDefinition { varName    = vname
                                  , varType    = varType
                                  , varDefault = init'
                                  , varPos     = pos
                                  , varIsLocal = True
                                  , varIsConst = const
                                  }
                )
              )
          return $ makeExprTyped
            (VarDeclaration (Var vname) (varType) const init')
            varType
            pos

      result <- (try $ typeVarDec) :: IO (Either KitError TypedExpr)
      case result of
        Left err -> do
          -- leave a dummy binding in the scope; since we're going to fail
          -- anyway, we may be able to get more type info and will avoid
          -- spurious "unknown identifier" errors downstream
          bindToScope
            (head $ tctxScopes tctx)
            (tpName vname)
            (VarBinding
              (newVarDefinition { varName    = vname
                                , varType    = inferredType ex
                                , varDefault = Nothing
                                , varPos     = pos
                                }
              )
            )
          throwk err
        Right x -> return x

    -- (Defer e1) -> do
    --   throwk $ InternalError "Not yet implemented" (Just pos)

    (StructInit structType@(TypeInstance tp p) fields) -> do
      let
        findStruct (TypeInstance tp p) tctx = do
          params <- makeGeneric ctx tp pos p
          tctx <- genericTctx ctx tctx pos (TypeInstance tp $ map snd params)
          params <- forMWithErrors (map snd params) $ mapType $ follow ctx tctx
          structDef <- getTypeDefinition ctx tp
          case typeSubtype structDef of
            Abstract { abstractUnderlyingType = parent@(TypeInstance tp p) } ->
              do
                parent <- mapType (follow ctx tctx) parent
                findStruct parent tctx

            Struct { structFields = structFields } -> do
              let providedNames = map fst fields
              let fieldNames    = map (tpName . varName) structFields
              let extraNames    = providedNames \\ fieldNames
              -- TODO: check for duplicate fields
              -- check for extra fields
              unless (null extraNames) $ throwk $ TypingError
                (  "Struct "
                ++ s_unpack (showTypePath tp)
                ++ " has the following extra fields:\n\n"
                ++ intercalate
                     "\n"
                     [ "  - `" ++ s_unpack name ++ "`" | name <- extraNames ]
                ++ "\n\nRemove these fields or correct any typos."
                )
                pos
              let nonProvidedNames = fieldNames \\ providedNames
              typedFields <- forMWithErrors
                (structFields)
                (\field -> do
                  fieldType <- mapType (follow ctx tctx) $ varType field
                  let
                    provided =
                      find (\(name, _) -> name == tpName (varName field)) fields
                  case provided of
                    Just (name, value) -> do
                      value <- typeExpr ctx tctx mod value
                      return $ Just ((name, value), fieldType)
                    Nothing -> case varDefault field of
                      Just fieldDefault -> do
                        fieldDefault <- typeExpr ctx tctx mod fieldDefault
                        return $ Just
                          ((tpName $ varName field, fieldDefault), fieldType)
                      Nothing -> case tctxState tctx of
                        TypingPattern -> return Nothing
                        _             -> throwk $ TypingError
                          (  "Struct "
                          ++ s_unpack (showTypePath tp)
                          ++ " is missing field "
                          ++ s_unpack (showTypePath $ varName field)
                          ++ ", and no default value is provided."
                          )
                          pos
                )
              typedFields <- forMWithErrors
                (catMaybes typedFields)
                (\((name, expr), fieldType) -> do
                  r1        <- r expr
                  converted <- tryAutoRefDeref ctx tctx fieldType r1
                  resolveConstraint ctx tctx $ TypeEq
                    fieldType
                    (inferredType converted)
                    "Struct field values must match the declared struct field type"
                    (tPos r1)
                  return (name, converted)
                )
              structType <- mapType (follow ctx tctx) $ TypeInstance tp params
              return $ (makeExprTyped (StructInit structType typedFields)
                                      structType
                                      pos
                       )
                { tIsLvalue = True
                , tIsLocal  = True
                }

            x -> throwk $ TypingError
              ("Type " ++ s_unpack (showTypePath tp) ++ " isn't a struct")
              pos

      result <- findStruct structType tctx
      return $ result { inferredType = structType }

    (TupleInit slots) -> do
      slots' <- forMWithErrors slots r
      return $ makeExprTyped (TupleInit slots')
                             (TypeTuple (map inferredType slots'))
                             pos

    (Implicit t) -> do
      t   <- mapType (follow ctx tctx) t
      val <- findImplicit ctx tctx t (tctxImplicits tctx)
      case val of
        Just (_, x) -> return x
        Nothing     -> throwk $ TypingError
          (  "Couldn't find an implicit value of type "
          ++ show t
          ++ ".\n\nImplicits in scope:\n\n"
          ++ intercalate
               "\n"
               [ "  - implicit " ++ (show $ inferredType i)
               | i <- tctxImplicits tctx
               ]
          )
          pos

    (Temp x) -> do
      rx <- r x
      return $ (makeExprTyped (Temp rx) (inferredType rx) (tPos rx)) { tIsLocal = True
                                                                     }

    (SizeOf t) -> do
      t <- mapType (follow ctx tctx) $ t
      return $ makeExprTyped (SizeOf t) (inferredType ex) pos

    _ -> return $ ex

  t'       <- mapType (follow ctx tctx) $ inferredType result
  result   <- return $ result { inferredType = t' }
  ownRules <- ownRules ctx tctx result
  result   <- return $ result { tImplicitRules = ownRules }
  tryRewrite result $ return result

ownRules ctx tctx this = case inferredType this of
  TypeBox tp params -> do
    def <- getTraitDefinition ctx tp
    return [ rule { ruleThis = Just this } | rule <- traitRules def ]
  TypeInstance tp params -> do
    def <- getTypeDefinition ctx tp
    let rules = [ rule { ruleThis = Just this } | rule <- typeRules def ]
    case typeSubtype def of
      Abstract { abstractUnderlyingType = parent@(TypeInstance tp params) } ->
        do
          parent      <- mapType (follow ctx tctx) parent
          parentRules <- ownRules ctx tctx $ this { inferredType = parent }
          return
            $  rules
            ++ [ rule { ruleThis = Just this } | rule <- parentRules ]
      _ -> return rules
  _ -> return []

alignCallArgs
  :: CompileContext
  -> TypeContext
  -> [ConcreteType]
  -> Bool
  -> [TypedExpr]
  -> [TypedExpr]
  -> IO [TypedExpr]
alignCallArgs ctx tctx []       isVariadic implicits args = return []
alignCallArgs ctx tctx argTypes isVariadic implicits args = do
  nextArg <- follow ctx tctx (head argTypes)
  found   <- findImplicit ctx tctx nextArg implicits
  case found of
    Just (x, y) -> do
      rest <- alignCallArgs ctx
                            tctx
                            (tail argTypes)
                            isVariadic
                            (delete x implicits)
                            args
      return $ y : rest
    Nothing -> return $ args

findImplicit
  :: CompileContext
  -> TypeContext
  -> ConcreteType
  -> [TypedExpr]
  -> IO (Maybe (TypedExpr, TypedExpr))
findImplicit ctx tctx ct []      = return Nothing
findImplicit ctx tctx ct (h : t) = do
  converted <- tryAutoRefDeref ctx tctx ct h
  match     <- unifyStrict ctx tctx ct (inferredType converted)
  case match of
    Just info ->
      if or
           [ case i of
               TypeVarIs _ _ -> True
               _             -> False
           | i <- info
           ]
        then findImplicit ctx tctx ct t
        else return $ Just (h, converted)
    Nothing -> findImplicit ctx tctx ct t

argDescriptions :: [ConcreteType] -> [ConcreteType] -> [String]
argDescriptions [] [] = []
argDescriptions [] explicits =
  [ (show $ length explicits)
      ++ " explicit argument"
      ++ plural (length explicits)
      ++ ":\n\n"
      ++ (intercalate "\n" [ "  - " ++ show t | t <- explicits ])
  ]
argDescriptions (MethodTarget t : implicits) explicits =
  ("`this` value:\n\n  - " ++ show t) : (argDescriptions implicits explicits)
argDescriptions implicits explicits =
  (  (show $ length implicits)
    ++ " implicit argument"
    ++ plural (length implicits)
    ++ ":\n\n"
    ++ (intercalate "\n" [ "  - " ++ show t | t <- implicits ])
    )
    : (argDescriptions [] explicits)

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
                             (isJust isVariadic)
                             implicits
                             args
    let usedImplicits = length aligned - length args
    when
        (if isJust isVariadic
          then length aligned < length argTypes
          else length aligned /= length argTypes
        )
      $ throwk
      $ TypingError
          (  "Function expected "
          ++ (show $ length argTypes)
          ++ (if isJust isVariadic then " or more" else "")
          ++ " argument"
          ++ (plural $ length argTypes)
          ++ ":\n\n  "
          ++ show ft
          ++ "\n\nCalled with "
          ++ let descriptions = argDescriptions
                   (map inferredType $ take usedImplicits aligned)
                   (map inferredType $ drop usedImplicits aligned)
             in  if null descriptions
                   then "no arguments"
                   else
                     (intercalate
                       "\n\n"
                       [ (if n > 0 then "and " else "") ++ d
                       | (n, d) <- zip [0 ..] descriptions
                       ]
                     )
          )
          pos
    converted <- forMWithErrors
      (zip (map Just argTypes ++ repeat Nothing) aligned)
      (\(arg, argValue) -> case arg of
        Just (_, argType) -> tryAutoRefDeref ctx tctx argType argValue
        Nothing           -> return argValue
      )
    forMWithErrors_
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
    return $ makeExprTyped
      (Call e (take usedImplicits converted) (drop usedImplicits converted))
      rt
      pos

typeEnumConstructorCall
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypedExpr
  -> [TypedExpr]
  -> TypePath
  -> TypePath
  -> ConcreteArgs
  -> [ConcreteType]
  -> IO TypedExpr
typeEnumConstructorCall ctx tctx mod e args tp discriminant argTypes params =
  do
    when (length args < length argTypes) $ throwk $ TypingError
      (  "Expected "
      ++ (show $ length argTypes)
      ++ " arguments (called with "
      ++ (show $ length args)
      ++ ")"
      )
      (tPos e)
    def <- getTypeDefinition ctx tp
    let tctx' = addTypeParams
          tctx
          [ (typeSubPath def $ paramName param, value)
          | (param, value) <- zip (typeParams def) params
          ]
    forMWithErrors_
      (zip argTypes args)
      (\((_, argType), argValue) -> do
        t1 <- follow ctx tctx' argType
        t2 <- follow ctx tctx' (inferredType argValue)
        resolveConstraint
          ctx
          tctx'
          (TypeEq t1
                  t2
                  "Enum arg types must match the enum's declaration"
                  (tPos argValue)
          )
      )
    let ct = TypeInstance tp params
    return $ makeExprTyped
      (EnumInit ct discriminant (zip (map fst argTypes) args))
      ct
      (tPos e)

typeStructUnionFieldAccess
  :: CompileContext
  -> TypeContext
  -> ConcreteType
  -> [VarDefinition TypedExpr ConcreteType]
  -> TypedExpr
  -> Str
  -> Span
  -> IO (Maybe TypedExpr)
typeStructUnionFieldAccess ctx tctx t@(TypeInstance tp params) fields r fieldName pos
  = do
    case findStructUnionField fields fieldName of
      Just field -> do
        let accessible = case tctxSelf tctx of
              Just (TypeInstance tp2 params2) | tp == tp2 -> True
              Nothing -> isPublic (varModifiers field)
        when (not accessible) $ throwk $ TypingError
          (  "Can't access non-public field "
          ++ s_unpack fieldName
          ++ " of "
          ++ show t
          )
          pos
        return
          $ ( Just
            $ (makeExprTyped (Field r (Var ([], fieldName)))
                             (varType field)
                             pos
              )
                { tIsLvalue = True
                , tIsConst  = varIsConst field
                }
            )

      Nothing -> return Nothing

findStructUnionField :: [VarDefinition a b] -> Str -> Maybe (VarDefinition a b)
findStructUnionField (h : t) fieldName = if (tpName $ varName h) == fieldName
  then Just h
  else findStructUnionField t fieldName
findStructUnionField [] _ = Nothing

typeVarBinding
  :: CompileContext -> TypeContext -> TypedBinding -> Span -> IO TypedExpr
typeVarBinding ctx tctx binding pos = do
  let invalidBinding = throwk $ InternalError "Invalid binding" (Just pos)
  let returnTypeBinding tp params = return
        $ makeExprTyped (Identifier (Var $ tp)) (TypeTypeOf tp params) pos
  let returnTraitBinding tp params = return $ makeExprTyped
        (Identifier (Var tp))
        (TypeTraitConstraint (tp, params))
        pos
  case binding of
    ExprBinding     x   -> return x
    EnumConstructor def -> do
      let parentTp     = variantParent def
      let discriminant = variantName def
      let extern       = hasMeta "extern" (variantMeta def)
      -- FIXME: pull params from tctx
      params <- makeGeneric ctx parentTp pos []
      let tctx' = addTypeParams tctx params
      let ct    = TypeInstance parentTp $ map snd params
      let args  = [ (argName arg, argType arg) | arg <- variantArgs def ]
      if null args
        then return $ makeExprTyped (EnumInit ct discriminant []) ct pos
        else do
          return $ makeExprTyped
            (Identifier $ Var discriminant)
            (TypeEnumConstructor parentTp discriminant args (map snd params))
            pos
    VarBinding v ->
      return $ (makeExprTyped (Identifier $ Var $ varRealName v) (varType v) pos
               )
        { tIsLvalue = True
        , tIsLocal  = varIsLocal v
        , tIsConst  = varIsConst v
        }
    FunctionBinding def -> do
      let t  = functionConcrete def
      let tp = functionRealName def
      if null $ functionParams def
        then return $ makeExprTyped (Identifier $ Var tp) t pos
        else do
  -- TODO: allow specifying explicit function params
          params <- makeGeneric ctx tp pos []
          let tctx' = addTypeParams tctx params
          t <- mapType (follow ctx tctx') t
          let ft = case t of
                TypeFunction rt args varargs _ ->
                  TypeFunction rt args varargs (map snd params)
          return $ makeExprTyped (Identifier $ Var tp) ft pos
    -- TODO: these are invalid runtime values; don't abuse Identifier for them
    TypeBinding  t -> returnTypeBinding (typeName t) []
    TraitBinding t -> returnTraitBinding (traitName t) []
    ModuleBinding tp ->
      return $ makeExprTyped (Identifier (Var tp)) (ModuleType tp) pos
    TypedefBinding t modPath _ -> do
      mod  <- getMod ctx modPath
      tctx <- modTypeContext ctx mod
      t    <- resolveType ctx tctx mod t
      case t of
        TypeInstance tp params -> returnTypeBinding tp params
        TypeTraitConstraint (tp, params) -> returnTraitBinding tp params
        _                      -> invalidBinding

exprToType
  :: CompileContext
  -> TypeContext
  -> Module
  -> Span
  -> TypedExpr
  -> IO (Maybe ConcreteType)
exprToType ctx tctx mod pos ex = do
  let r x = exprToType ctx tctx mod pos x
  x <- case (tExpr ex, inferredType ex) of
    (_, TypeTypeOf tp params) -> return $ Just $ TypeInstance tp params
    (Identifier (Var s), _) -> do
      t <- resolveType ctx tctx mod $ TypeSpec s [] pos
      return $ Just t
    (TupleInit x, _) -> do
      x <- forM x $ r
      let result = sequence x
      case result of
        Just x -> return $ Just $ TypeTuple $ reverse x
        _      -> return Nothing
    (ArrayAccess a b, _) -> do
      b <- r b
      case b of
        Just b -> do
          a <- r a
          case a of
            Just (TypeInstance tp params) ->
              return $ Just $ TypeInstance tp $ params ++ [b]
            _ -> return Nothing
        _ -> return Nothing
    (PreUnop Deref x, _) -> do
      x <- r x
      case x of
        Just x -> return $ Just $ TypePtr x
        _      -> return Nothing
    _ -> return Nothing
  case x of
    Just t -> genericTctx ctx tctx pos t >> return ()
    _      -> return ()
  return x
