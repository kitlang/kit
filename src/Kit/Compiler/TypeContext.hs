module Kit.Compiler.TypeContext where

import           Control.Exception
import           Control.Monad
import           Data.Mutable
import           Kit.Ast
import           Kit.Compiler.Binding
import           Kit.Compiler.Context
import           Kit.Compiler.Module
import           Kit.Compiler.Scope
import           Kit.Compiler.Utils
import           Kit.Error
import           Kit.HashTable
import           Kit.Parser
import           Kit.Str

data TypingError = TypingError String Span deriving (Eq, Show)
instance Errable TypingError where
  logError e@(TypingError s _) = logErrorBasic (KitError e) s
  errPos (TypingError _ pos) = Just pos

type TypedStmtWithContext = (TypedStmt, TypeContext)

data TypeContext = TypeContext {
  tctxScopes :: ![Scope TypedBinding],
  tctxMacroVars :: ![(Str, TypedExpr)],
  tctxRules :: [RuleSet TypedExpr ConcreteType],
  tctxActiveRules :: [(RewriteRule TypedExpr ConcreteType, Span)],
  tctxReturnType :: Maybe ConcreteType,
  tctxVarargsParameter :: Maybe Str,
  tctxThis :: Maybe ConcreteType,
  tctxSelf :: Maybe ConcreteType,
  tctxImplicits :: ![TypedExpr],
  tctxTypeParams :: ![(TypePath, ConcreteType)],
  tctxLoopCount :: !Int,
  tctxRewriteRecursionDepth :: !Int,
  tctxState :: TypeContextState
}

data TypeContextState
  = TypingExpression
  | TypingPattern
  | TypingExprOrType

newTypeContext :: [Scope TypedBinding] -> IO TypeContext
newTypeContext scopes = do
  return $ TypeContext
    { tctxScopes                = scopes
    , tctxRules                 = []
    , tctxActiveRules           = []
    , tctxMacroVars             = []
    , tctxReturnType            = Nothing
    , tctxThis                  = Nothing
    , tctxSelf                  = Nothing
    , tctxImplicits             = []
    , tctxTypeParams            = []
    , tctxLoopCount             = 0
    , tctxRewriteRecursionDepth = 0
    , tctxState                 = TypingExpression
    , tctxVarargsParameter      = Nothing
    }

unknownType t pos = do
  throw $ KitError $ TypingError ("Unknown type: " ++ (show t)) pos

resolveTypeParam :: TypePath -> [(TypePath, ConcreteType)] -> Maybe ConcreteType
resolveTypeParam s (h : t) =
  if (snd (fst h) == snd s) && (null (fst s) || (fst (fst h) == fst s))
    then case snd h of
      TypeTypeParam x -> case resolveTypeParam x t of
        Just x  -> Just x
        Nothing -> Just $ snd h
      x -> Just x
    else resolveTypeParam s t
resolveTypeParam s [] = Nothing

bindingToCt :: Maybe (Binding a b) -> [ConcreteType] -> Maybe ConcreteType
bindingToCt binding params = case binding of
  Just (TypeBinding    t) -> Just $ TypeInstance (typeName t) params
  Just (TraitBinding   t) -> Just $ TypeTraitConstraint (traitName t, params)
  Just (RuleSetBinding r) -> Just $ TypeRuleSet (ruleSetName r)
  _                       -> Nothing

findBindingCt
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypePath
  -> [ConcreteType]
  -> IO (Maybe ConcreteType)
findBindingCt ctx tctx mod tp params = do
  full <- h_lookup (ctxBindings ctx) tp
  case full of
    Just (TypedefBinding tp modPath _) -> do
      mod  <- getMod ctx modPath
      tctx <- modTypeContext ctx mod
      t    <- resolveType ctx tctx mod tp
      return $ Just t
    Just x -> return $ bindingToCt (Just x) params
    _      -> do
      header <- h_lookup (ctxTypes ctx) tp
      return $ bindingToCt header params

findLocalOrImportedCt
  :: CompileContext
  -> TypeContext
  -> Module
  -> Str
  -> [ConcreteType]
  -> IO (Maybe ConcreteType)
findLocalOrImportedCt ctx tctx mod s resolvedParams = do
  local <- findBindingCt ctx tctx mod (modPath mod, s) resolvedParams
  case local of
    Just x  -> return $ Just x
    Nothing -> do
      -- search other modules
      bound <- foldM
        (\acc v -> case acc of
          Just x  -> return acc
          Nothing -> findBindingCt ctx tctx mod (v, s) resolvedParams
        )
        Nothing
        (modImportPaths mod)
      case bound of
        Just t -> return $ Just t
        _      -> do
          t <- h_lookup (ctxTypedefs ctx) s
          case t of
            Just x -> do
              x <- follow ctx tctx x
              return $ Just x
            Nothing -> return Nothing

{-
  Attempt to resolve a TypeSpec into a ConcreteType; fail if it isn't a known
  type.
-}
resolveType
  :: CompileContext -> TypeContext -> Module -> TypeSpec -> IO ConcreteType
resolveType ctx tctx mod t = do
  veryNoisyDebugLog ctx $ "resolve type: " ++ show t
  case t of
    ConcreteType ct        -> follow ctx tctx ct
    ConstantTypeSpec v pos -> return $ ConstantType v
    TupleTypeSpec    t pos -> do
      slots <- forM t (resolveType ctx tctx mod)
      return $ TypeTuple slots
    PointerTypeSpec t pos -> do
      t <- resolveType ctx tctx mod t
      return $ TypePtr t
    TypeSpec (m, s) params pos -> do
      builtin <- if null m && null params
        then builtinToConcreteType ctx tctx mod s pos
        else return Nothing
      ct <- case builtin of
        Just t  -> follow ctx tctx t
        Nothing -> do
          resolvedParams <- forM params (resolveType ctx tctx mod)
          case m of
            ["extern"] -> do
              binding <- findBindingCt ctx tctx mod ([], s) resolvedParams
              case binding of
                Just x  -> return x
                Nothing -> do
                  t <- h_lookup (ctxTypedefs ctx) s
                  case t of
                    Just x  -> follow ctx tctx x
                    Nothing -> unknownType (s_unpack $ showTypePath (m, s)) pos
            [] -> do
              case (s, tctxSelf tctx) of
                ("Self", Just self) -> return self
                ("Self", Nothing) ->
                  throwk $ TypingError "No Self type in this context" pos
                _ -> do
                  case resolveTypeParam ([], s) (tctxTypeParams tctx) of
                    Just x -> return x
                    _      -> do
                      ct <- findLocalOrImportedCt ctx tctx mod s resolvedParams
                      case ct of
                        Just ct -> do
                          -- if this is a type instance, create a new generic
                          makeGenericConcrete ctx (position t) ct
                        Nothing ->
                          unknownType (s_unpack $ showTypePath (m, s)) pos
            m -> do
              -- search only a specific module for this type
              result <- h_lookup (ctxTypes ctx) (m, s)
              case bindingToCt result resolvedParams of
                Just x  -> return x
                Nothing -> unknownType (s_unpack $ showTypePath (m, s)) pos
      veryNoisyDebugLog ctx $ "resolved to " ++ show ct
      follow ctx tctx ct

    FunctionTypeSpec rt args isVariadic pos -> do
      rt'   <- resolveType ctx tctx mod rt
      -- FIXME: arg names
      args' <- forM
        (args)
        (\arg -> do
          t <- resolveType ctx tctx mod arg
          return $ newArgSpec { argName = "_", argType = t }
        )
      return $ TypeFunction rt' args' isVariadic []

    InferredType pos ->
      throwk $ BasicError ("Inferred type isn't supported here") (Just pos)

resolveMaybeType
  :: CompileContext
  -> TypeContext
  -> Module
  -> [TypePath]
  -> Span
  -> TypeSpec
  -> IO ConcreteType
resolveMaybeType ctx tctx mod params pos t = do
  case t of
    InferredType _ -> if null params
      then makeTypeVar ctx pos
      else makeTemplateVar ctx params pos
    t -> resolveType ctx tctx mod t

follow :: CompileContext -> TypeContext -> ConcreteType -> IO ConcreteType
follow ctx tctx t = do
  veryNoisyDebugLog ctx $ "follow started"
  _follow ctx tctx 256 t
_follow
  :: CompileContext -> TypeContext -> Int -> ConcreteType -> IO ConcreteType
_follow ctx tctx count t = do
  let r x = _follow ctx tctx (count - 1) x
  when (count < 1) $ throwk $ InternalError
    ("Maximum recursion depth in follow exceeded; last input: " ++ show t)
    Nothing
  veryNoisyDebugLog ctx $ "follow " ++ show t
  case t of
    TypeBool                        -> return t
    TypeChar                        -> return t
    TypeSize                        -> return t
    TypeInt   _                     -> return t
    TypeUint  _                     -> return t
    TypeFloat _                     -> return t
    UnresolvedType typeSpec modPath -> do
      mod <- getMod ctx modPath
      resolveType ctx tctx mod typeSpec
    TypeSelf -> do
      case tctxSelf tctx of
        Just TypeSelf -> return TypeSelf
        Just x        -> r x
        Nothing       -> return TypeSelf
    TypeTypeParam p -> do
      case resolveTypeParam p (tctxTypeParams tctx) of
        Just (TypeTypeParam q) | p == q -> return $ TypeTypeParam p
        Just x                          -> r x
        Nothing                         -> return $ TypeTypeParam p
    TypeTypeVar x -> do
      info <- getTypeVar ctx x
      case typeVarValue info of
        -- Specific known type
        Just t  -> r t
        -- No specific type; return type var
        Nothing -> return t
    TypeTemplateVar requiredParams i pos -> do
      params <- forMWithErrors requiredParams $ \tp -> do
        case resolveTypeParam tp (tctxTypeParams tctx) of
          Just t -> do
            return t
          Nothing -> do
            throwk $ TypingError
              (  "Required type parameter not in scope: "
              ++ s_unpack (showTypePath tp)
              )
              pos
      i <- templateVarToTypeVar ctx i params pos
      r $ TypeTypeVar i
    TypeInstance tp params -> do
      resolvedParams <- forM params (r)
      return $ TypeInstance tp resolvedParams
    TypePtr t -> do
      t <- r t
      return $ TypePtr t
    TypeFunction t args varargs params -> do
      resolved     <- r t
      resolvedArgs <- forM
        args
        (\arg -> do
          resolvedArg <- r $ argType arg
          return $ arg { argType = resolvedArg }
        )
      resolvedParams <- forM params (r)
      return $ TypeFunction resolved resolvedArgs varargs resolvedParams
    TypeEnumConstructor tp d args params -> do
      resolvedArgs <- forM
        args
        (\arg -> do
          resolvedArg <- r $ argType arg
          return $ arg { argType = resolvedArg }
        )
      resolvedParams <- forM params (r)
      return $ TypeEnumConstructor tp d resolvedArgs resolvedParams
    TypeTypedef "__builtin_va_list" ->
      return $ TypeBasicType $ BasicTypeTypedef "__builtin_va_list"
    TypeTypedef name -> do
      typedef <- h_lookup (ctxTypedefs ctx) name
      case typedef of
        Just t  -> r t
        Nothing -> throwk $ InternalError
          ("Unexpected missing typedef: " ++ (s_unpack name))
          Nothing
    TypeTuple t -> do
      resolvedT <- forM t $ r
      return $ TypeTuple resolvedT
    TypeTraitConstraint (tp, params) -> do
      resolvedParams <- forM params (r)
      return $ TypeTraitConstraint (tp, resolvedParams)
    TypeAny      pos -> makeTypeVar ctx pos
    MethodTarget t   -> do
      t <- r t
      return $ MethodTarget t
    _ -> return t

followType ctx tctx = convertTypeDefinition
  (\_ -> return $ converter return (\_ -> follow ctx tctx))
followFunction ctx tctx = convertFunctionDefinition
  (\_ -> return $ converter return (\_ -> follow ctx tctx))
followTrait ctx tctx = convertTraitDefinition
  (\_ -> return $ converter return (\_ -> follow ctx tctx))
followVariant ctx tctx =
  convertEnumVariant (converter return (\_ -> follow ctx tctx))

addUsing
  :: CompileContext
  -> TypeContext
  -> UsingType TypedExpr ConcreteType
  -> IO TypeContext
addUsing ctx tctx using = case using of
  UsingRuleSet (TypeRuleSet tp) -> do
    def <- lookupBinding ctx tp
    case def of
      Just (RuleSetBinding r) -> do
        return $ tctx { tctxRules = r : tctxRules tctx }
      _ -> throwk $ InternalError
        ("Missing ruleset from using: " ++ s_unpack (showTypePath tp))
        Nothing
  UsingImplicit x -> case tctxImplicits tctx of
    y' : _ | x == y' -> return tctx
    y                -> return $ tctx { tctxImplicits = x : y }
  _ -> return tctx

addTypeParams
  :: CompileContext
  -> TypeContext
  -> [(TypePath, ConcreteType)]
  -> Span
  -> IO TypeContext
addTypeParams ctx tctx params pos = do
  scope <- newScope
  forM_ params $ \(param, pt) -> do
    case pt of
      ConstantType v -> do
        t  <- h_lookup (ctxConstantParamTypes ctx) param
        tv <- case t of
          Just x  -> return x
          Nothing -> do
            tv <- makeTypeVar ctx pos
            h_insert (ctxConstantParamTypes ctx) param tv
            return tv
        bindToScope scope (tpName param)
          $ ExprBinding
          $ (makeExprTyped (Literal v tv) tv pos) { tIsConst          = True
                                                  , tCompileTimeValue = Just v
                                                  }
      TypeInstance tp params ->
        bindToScope scope (tpName param) $ ExprBinding $ makeExprTyped
          (Identifier (Var param))
          (TypeTypeOf tp params)
          NoPos
      _ -> return ()
  return $ tctx { tctxScopes     = scope : tctxScopes tctx
                , tctxTypeParams = params ++ tctxTypeParams tctx
                }

modTypeContext :: CompileContext -> Module -> IO TypeContext
modTypeContext ctx mod = do
  tctx  <- newTypeContext []
  using <- readRef (modUsing mod)
  foldM (\tctx using -> addUsing ctx tctx using) tctx using

builtinToConcreteType
  :: CompileContext
  -> TypeContext
  -> Module
  -> Str
  -> Span
  -> IO (Maybe ConcreteType)
builtinToConcreteType ctx tctx mod s pos = do
  case s of
    "Void" -> return $ Just $ TypeBasicType BasicTypeVoid
    _      -> return Nothing

getTraitImpl
  :: CompileContext
  -> TypeContext
  -> TraitConstraint
  -> ConcreteType
  -> IO (Maybe (TraitImplementation TypedExpr ConcreteType))
getTraitImpl ctx tctx trait@(traitTp, params) ct = do
  traitDef <- getTraitDefinition ctx traitTp
  ct       <- follow ctx tctx ct
  let explicitParams = traitExplicitParams traitDef params
  traitImpls <- h_lookup (ctxImpls ctx) (traitTp, explicitParams)
  result     <- case traitImpls of
    Just x -> do
      lookup <- h_lookup x ct
      case lookup of
        Just y -> return $ Just y
        _      -> return Nothing
    Nothing -> return Nothing
  case result of
    Just impl -> return $ Just impl
    Nothing   -> do
      case ct of
        TypeInstance tp params -> do
          def <- getTypeDefinition ctx tp
          case typeSubtype def of
            Abstract { abstractUnderlyingType = u } -> do
              tctx <- addTypeParams
                ctx
                tctx
                [ (typeSubPath def (paramName param), value)
                | (param, value) <- zip (typeParams def) params
                ]
                (typePos def)
              getTraitImpl ctx tctx trait u
            _ -> return Nothing
        _ -> return Nothing

functionConcrete f =
  TypeFunction (functionType f) (functionArgs f) (functionVararg f) []

{-
  Given a ConcreteType, returns a new TypeContext which has instantiated any
  generics contained in the type, recursively, allowing type param values to
  be looked up.
-}
genericTctx
  :: CompileContext -> TypeContext -> Span -> ConcreteType -> IO TypeContext
genericTctx ctx tctx pos t = do
  t <- follow ctx tctx t
  case t of
    TypeTypeOf tp params -> genericTctx ctx tctx pos (TypeInstance tp params)
    TypePtr t            -> genericTctx ctx tctx pos t
    TypeBox tp params ->
      genericTctx ctx tctx pos (TypeTraitConstraint (tp, params))
    TypeInstance tp params -> do
      params  <- forMWithErrors params $ follow ctx tctx
      params  <- makeGeneric ctx tp pos params
      tctx    <- addTypeParams ctx tctx params pos
      binding <- lookupBinding ctx tp
      -- if this is an abstract, make a generic instance of its parent;
      -- since we might call this before all type definitions are available,
      -- fall back to checking ctxTypes
      case binding of
        Just (TypeBinding def) -> do
          case typeSubtype def of
            Abstract { abstractUnderlyingType = parent } ->
              genericTctx ctx tctx pos parent
            _ -> return tctx
        _ -> do
          (TypeBinding def) <- h_get (ctxTypes ctx) tp
          case typeSubtype def of
            Abstract { abstractUnderlyingType = parent } -> do
              mod    <- getMod ctx (fst tp)
              parent <- resolveType ctx tctx mod parent
              genericTctx ctx tctx pos parent
            _ -> return tctx
    TypeTraitConstraint (tp, params) -> do
      params <- forMWithErrors params $ follow ctx tctx
      params <- makeGeneric ctx tp pos params
      addTypeParams ctx tctx params pos
    TypeTuple t -> do
      foldM_ (\tctx -> genericTctx ctx tctx pos) tctx t
      return tctx
    _ -> return tctx
