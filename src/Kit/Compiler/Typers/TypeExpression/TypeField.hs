module Kit.Compiler.Typers.TypeExpression.TypeField
  ( typeField
  )
where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Kit.Ast
import           Kit.Compiler.Binding
import           Kit.Compiler.Context
import           Kit.Compiler.Module
import           Kit.Compiler.TypeContext
import           Kit.Compiler.Typers.AutoRefDeref
import           Kit.Compiler.Typers.ExprTyper
import           Kit.Compiler.Typers.TypeExpression.TypeVarBinding
import           Kit.Error
import           Kit.Str

typeField :: SubTyper
typeField (TyperUtils { _r = r, _tryRewrite = tryRewrite, _resolve = resolve, _typeExpr = typeExpr }) ctx tctx mod ex@(TypedExpr { tExpr = et, tPos = pos })
  = case et of
    (Field e1 (Var fieldName)) -> do
      r1 <- r e1
      tryRewrite
          (makeExprTyped (Field r1 (Var fieldName)) (inferredType ex) pos)
        $ do
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
                        params <- forMWithErrors (map snd params)
                          $ follow ctx tctx
                        tctx <- return
                          $ tctx { tctxSelf = Just $ TypeInstance tp params }
                        let accessible = case tctxSelf tctx of
                              Just (TypeInstance tp2 params2) | tp2 == tp ->
                                True
                              _ -> bindingIsPublic binding
                        when (not accessible) failNotPublic
                        x <- typeVarBinding ctx tctx binding pos
                        f <- follow ctx tctx $ inferredType x
                        -- when calling an instance method statically, remove MethodTarget
                        f <- return $ case f of
                          TypeFunction rt ((arg@(ArgSpec { argType = MethodTarget t }) : args)) varargs params
                            -> TypeFunction rt
                                            ((arg { argType = t }) : args)
                                            varargs
                                            params
                          _ -> f
                        case binding of
                          FunctionBinding _ ->
                            return
                              $ (makeExprTyped
                                  ((StaticMember tp params) fieldName)
                                  f
                                  pos
                                )
                                  { tIsLvalue = True
                                  }
                          VarBinding _ ->
                            return
                              $ (makeExprTyped
                                  ((StaticMember tp params) fieldName)
                                  f
                                  pos
                                )
                                  { tIsLvalue = True
                                  }
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
                            (  "Couldn't find trait: "
                            ++ s_unpack (showTypePath tp)
                            )
                            pos
                        tctx <- addTypeParams
                          ctx
                          (tctx { tctxSelf = Just t })
                          [ (traitSubPath traitDef $ paramName param, val)
                          | (param, val) <- zip (traitAllParams traitDef) params
                          ]
                          pos

                        method <- lookupBinding ctx $ subPath tp fieldName
                        case method of
                          Just binding -> do
                            x <- typeVarBinding ctx tctx binding pos
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
                            -- trait <- followTrait ctx tctx (modPath mod) traitDef
                            t <- follow ctx tctx (inferredType x)
                            return $ typed
                              { tImplicits   = [ makeExprTyped
                                                   (BoxedValue r1)
                                                   ( MethodTarget
                                                   $ TypePtr TypeVoid
                                                   )
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
                        t      <- follow ctx tctx $ inferredType result
                        return
                          $ (makeExprTyped (Field r1 $ Var ([], fieldName))
                                           t
                                           pos
                            )
                              { tImplicits = if null $ tImplicits r1
                                             then
                                               [ ref
                                                   { inferredType = (MethodTarget
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
                    tctx        <- addTypeParams
                      ctx
                      (tctx { tctxSelf = Just t })
                      [ (typeSubPath templateDef $ paramName param, val)
                      | (param, val) <- zip (typeParams templateDef) params
                      ]
                      (typePos templateDef)
                    def <- followType ctx tctx templateDef
                    let subtype = typeSubtype def
                    let
                      fail = case subtype of
                        StructUnion { structUnionFields = fields, isStruct = isStruct }
                          -> do
                            result <- typeStructUnionFieldAccess ctx
                                                                 tctx
                                                                 t
                                                                 fields
                                                                 r1
                                                                 fieldName
                                                                 pos
                            case result of
                              Just x -> return $ x { tIsLvalue = True }
                              _      -> throwk $ TypingError
                                (  (if isStruct then "Struct " else "Union ")
                                ++ s_unpack (showTypePath tp)
                                ++ " doesn't have a field called `"
                                ++ s_unpack fieldName
                                ++ "`"
                                )
                                pos

                        Enum { enumVariants = variants } -> do
                          case
                              find (((==) fieldName) . tpName . variantName)
                                   variants
                            of
                              Just v -> do
                                resolve $ TypeEq
                                  (TypeEnumVariant tp
                                                   (tpName $ variantName v)
                                                   params
                                  )
                                  (inferredType ex)
                                  "Struct field access must match the field's type"
                                  (tPos r1)
                                return $ r1
                                  { inferredType = (TypeEnumVariant
                                                     tp
                                                     (tpName $ variantName v)
                                                     params
                                                   )
                                  }
                              Nothing -> throwk $ TypingError
                                (  "Enum "
                                ++ (s_unpack $ showTypePath tp)
                                ++ " doesn't have a variant called "
                                ++ s_unpack fieldName
                                )
                                pos

                        Abstract { abstractUnderlyingType = u } ->
                          -- forward to parent
                          typeFieldAccess u fieldName

                    binding <- lookupBinding ctx $ subPath tp fieldName
                    case binding of
                      Just x -> do
                        let accessible = case tctxSelf tctx of
                              Just (TypeInstance tp2 params2) | tp2 == tp ->
                                True
                              _ -> bindingIsPublic x
                        when (not accessible) failNotPublic
                        -- this is a local method
                        typed' <- typeVarBinding ctx tctx x pos
                        t      <- follow ctx tctx (inferredType typed')
                        let typed = typed' { inferredType = t }
                        -- this may be a template; replace `this` with the actual
                        -- type to guarantee the implicit pass will work
                        case inferredType typed of
                          TypeFunction rt args varargs _ ->
                            let
                              f = TypeFunction
                                rt
                                ( (let arg = (head args)
                                   in
                                     arg
                                       { argType = MethodTarget
                                         $ TypePtr
                                         $ inferredType r1
                                       }
                                  )
                                : (tail args)
                                )
                                varargs
                                params
                            in
                              return
                                $ (makeExprTyped
                                    (StaticMember tp params fieldName)
                                    f
                                    pos
                                  )
                                    { tImplicits = (r1
                                                     { inferredType = MethodTarget
                                                       (inferredType r1)
                                                     }
                                                   )
                                      : tImplicits typed
                                    }
                          _ -> fail
                      _ -> fail

                  TypeEnumVariant tp specificVariant params -> do
                    templateDef <- getTypeDefinition ctx tp
                    tctx        <- addTypeParams
                      ctx
                      (tctx { tctxSelf = Just t })
                      [ (typeSubPath templateDef $ paramName param, val)
                      | (param, val) <- zip (typeParams templateDef) params
                      ]
                      (typePos templateDef)
                    def <- followType ctx tctx templateDef
                    let subtype = typeSubtype def
                    case subtype of
                      Enum { enumVariants = variants } -> do
                        case
                            find
                              (((==) specificVariant) . tpName . variantName)
                              variants
                          of
                            Just variant -> do
                              case
                                  find (((==) fieldName) . argName)
                                       (variantArgs variant)
                                of
                                  Just arg -> do
                                    t <- follow ctx tctx $ argType arg
                                    return $ makeExprTyped
                                      (EnumField
                                        (r1
                                          { inferredType = TypeInstance tp
                                                                        params
                                          }
                                        )
                                        specificVariant
                                        fieldName
                                      )
                                      t
                                      pos
                                  Nothing -> throwk $ TypingError
                                    (  "Enum variant "
                                    ++ (s_unpack $ showTypePath tp)
                                    ++ "."
                                    ++ s_unpack specificVariant
                                    ++ " doesn't have a field called "
                                    ++ s_unpack fieldName
                                    )
                                    pos

                            Nothing -> throwk $ TypingError
                              (  "Enum "
                              ++ (s_unpack $ showTypePath tp)
                              ++ " doesn't have a variant called "
                              ++ s_unpack specificVariant
                              )
                              pos
                      _ -> throwk $ InternalError
                        ("EnumVariant on non-enum: " ++ show subtype)
                        (Just pos)

                  TypeAnonStruct _ fields ->
                    case
                        find
                          (\(structFieldName, _) -> structFieldName == fieldName
                          )
                          fields
                      of
                        Just (fieldName, fieldType) -> do
                          resolve $ TypeEq
                            fieldType
                            (inferredType ex)
                            "Struct field access must match the field's type"
                            (tPos r1)
                          return $ makeExprTyped
                            (Field r1 $ Var ([], fieldName))
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
                        find
                          (\(unionFieldName, _) -> unionFieldName == fieldName)
                          fields
                      of
                        Just (fieldName, fieldType) -> do
                          resolve $ TypeEq
                            fieldType
                            (inferredType ex)
                            "Union field access must match the field's type"
                            (tPos r1)
                          return $ makeExprTyped
                            (Field r1 $ Var ([], fieldName))
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
                        (  "Unknown identifier "
                        ++ s_unpack (showTypePath newPath)
                        )
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
                let
                  fail = do
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
                binding <- resolveVar ctx
                                      (tctxScopes tctx)
                                      mod
                                      (tpName fieldName)
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
                      TypeFunction rt ((arg@(ArgSpec { argType = t }) : args)) varargs params
                        -> TypeFunction
                          rt
                          ((arg { argType = MethodTarget t }) : args)
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
