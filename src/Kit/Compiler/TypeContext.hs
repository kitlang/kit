module Kit.Compiler.TypeContext where

import Control.Exception
import Control.Monad
import Data.IORef
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Parser
import Kit.Str

data TypingError = TypingError String Span deriving (Eq, Show)
instance Errable TypingError where
  logError e@(TypingError s _) = logErrorBasic (KitError e) s
  errPos (TypingError _ pos) = Just pos

type TypedDeclWithContext = (TypedDecl, TypeContext)

data TypeContext = TypeContext {
  tctxScopes :: [Scope Binding],
  tctxMacroVars :: [(Str, TypedExpr)],
  tctxRules :: [RuleSet TypedExpr ConcreteType],
  tctxActiveRules :: [(RewriteRule TypedExpr ConcreteType, Span)],
  tctxReturnType :: Maybe ConcreteType,
  tctxThis :: Maybe ConcreteType,
  tctxSelf :: Maybe ConcreteType,
  tctxImplicits :: [TypedExpr],
  tctxTypeParams :: [(TypePath, ConcreteType)],
  tctxLoopCount :: Int,
  tctxRewriteRecursionDepth :: Int,
  tctxState :: TypeContextState
}

data TypeContextState
  = TypingExpression
  | TypingPattern

newTypeContext :: [Scope Binding] -> IO TypeContext
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
    }

unknownType t pos = do
  throw $ KitError $ TypingError ("Unknown type: " ++ (show t)) pos

resolveTypeParam :: TypePath -> [(TypePath, ConcreteType)] -> Maybe ConcreteType
resolveTypeParam s (h : t) =
  if (snd (fst h) == snd s) && (null (fst s) || (fst (fst h) == fst s))
    then Just $ snd h
    else resolveTypeParam s t
resolveTypeParam s [] = Nothing

{-
  Attempt to resolve a TypeSpec into a ConcreteType; fail if it isn't a known
  type.
-}
resolveType
  :: CompileContext -> TypeContext -> Module -> TypeSpec -> IO ConcreteType
resolveType ctx tctx mod t = do
  importedMods <- getModImports ctx mod
  case t of
    ConcreteType ct        -> follow ctx tctx ct
    ConstantTypeSpec v pos -> return $ ConstantType v
    TupleTypeSpec    t pos -> do
      slots <- forM t (resolveType ctx tctx mod)
      return $ TypeTuple slots
    TypeSpec (m, s) params pos -> do
      builtin <- if null m
        then builtinToConcreteType ctx tctx mod s params pos
        else return Nothing
      case builtin of
        Just t  -> follow ctx tctx t
        Nothing -> do
          resolvedParams <- forM params (resolveType ctx tctx mod)
          let bindingToCt binding = case binding of
                Just (TypeBinding t) ->
                  Just $ TypeInstance (typeName t) resolvedParams
                Just (TraitBinding t) ->
                  Just $ TypeTraitConstraint (traitName t, resolvedParams)
                Just (VarBinding     v) -> Just $ varType v
                Just (RuleSetBinding r) -> Just $ TypeRuleSet (ruleSetName r)
                Just (ExprBinding    x) -> Just $ inferredType x
                _                       -> Nothing
          case m of
            ["extern"] -> do
              binding <- lookupBinding ctx ([], s)
              case bindingToCt binding of
                Just x  -> return x
                Nothing -> unknownType (s_unpack $ showTypePath (m, s)) pos
            [] -> do
              case (s, tctxSelf tctx) of
                ("Self", Just self) -> return self
                _                   -> do
                  case resolveTypeParam ([], s) (tctxTypeParams tctx) of
                    Just x -> return x
                    _      -> do
                      scoped <- resolveBinding (tctxScopes tctx) s
                      ct     <- case bindingToCt scoped of
                        Just x -> return x
                        _      -> do
                          -- search other modules
                          imports <- getModImports ctx mod
                          bound   <- foldM
                            (\acc v -> case acc of
                              Just x  -> return acc
                              Nothing -> do
                                b <- lookupBinding ctx (v, s)
                                return $ bindingToCt b
                            )
                            Nothing
                            imports
                          case bound of
                            Just t -> return t
                            _      -> unknownType s pos
                      -- if this is a type instance, create a new generic
                      case ct of
                        TypeInstance tp@(modPath, name) p -> do
                          params <- makeGeneric ctx tp (typeSpecPosition t) p
                          return $ TypeInstance tp (map snd params)
                        TypeBox tp@(modPath, name) p -> do
                          params <- makeGeneric ctx tp (typeSpecPosition t) p
                          return $ TypeBox tp (map snd params)
                        TypeTraitConstraint (tp@(modPath, name), p) -> do
                          params <- makeGeneric ctx tp (typeSpecPosition t) p
                          return $ TypeTraitConstraint (tp, (map snd params))
                        _ -> return ct
            m -> do
              -- search only a specific module for this type
              result <- lookupBinding ctx (m, s)
              case result of
                Just (TypeBinding t) ->
                  follow ctx tctx $ TypeInstance (typeName t) []
                Just (TypedefBinding t _) -> follow ctx tctx t
                _ -> unknownType (s_unpack $ showTypePath (m, s)) pos

    FunctionTypeSpec rt args isVariadic pos -> do
      rt'   <- resolveType ctx tctx mod rt
      -- FIXME: arg names
      args' <- forM
        (args)
        (\arg -> do
          t <- resolveType ctx tctx mod arg
          return ("_", t)
        )
      return $ TypeFunction rt' args' isVariadic []

resolveMaybeType
  :: CompileContext
  -> TypeContext
  -> Module
  -> Span
  -> Maybe TypeSpec
  -> IO ConcreteType
resolveMaybeType ctx tctx mod pos t = do
  case t of
    Just t  -> resolveType ctx tctx mod t
    Nothing -> makeTypeVar ctx pos

follow :: CompileContext -> TypeContext -> ConcreteType -> IO ConcreteType
follow ctx tctx t = do
  veryNoisyDebugLog ctx $ "follow " ++ show t
  case t of
    TypeSelf -> do
      case tctxSelf tctx of
        Just TypeSelf -> return TypeSelf
        Just x        -> follow ctx tctx x
        Nothing       -> return TypeSelf
    TypeTypeParam p -> do
      case resolveTypeParam p (tctxTypeParams tctx) of
        Just (TypeTypeParam q) | p == q -> return $ TypeTypeParam p
        Just x                          -> follow ctx tctx x
        Nothing                         -> return $ TypeTypeParam p
    TypeTypeVar x -> do
      info <- getTypeVar ctx x
      case typeVarValue info of
        -- Specific known type
        Just t  -> follow ctx tctx t
        -- No specific type; return type var
        Nothing -> return t
    TypeInstance tp params -> do
      resolvedParams <- forM params (follow ctx tctx)
      return $ TypeInstance tp resolvedParams
    TypeFunction t args varargs params -> do
      resolved     <- follow ctx tctx t
      resolvedArgs <- forM
        args
        (\(name, t) -> do
          resolvedArg <- follow ctx tctx t
          return (name, resolvedArg)
        )
      resolvedParams <- forM params (follow ctx tctx)
      return $ TypeFunction resolved resolvedArgs varargs resolvedParams
    TypePtr t -> do
      resolved <- follow ctx tctx t
      return $ TypePtr resolved
    TypeBox tp params -> do
      resolvedParams <- forM params (follow ctx tctx)
      return $ TypeBox tp resolvedParams
    TypeArray t len -> do
      resolved <- follow ctx tctx t
      return $ TypeArray resolved len
    TypeEnumConstructor tp d args params -> do
      resolvedArgs <- forM
        args
        (\(name, t) -> do
          resolvedArg <- follow ctx tctx t
          return (name, resolvedArg)
        )
      resolvedParams <- forM params (follow ctx tctx)
      return $ TypeEnumConstructor tp d resolvedArgs resolvedParams
    TypeTypedef tp params -> do
      binding <- lookupBinding ctx tp
      case binding of
        Just (TypedefBinding t _) -> follow ctx tctx t
        Just (TypeBinding t) -> follow ctx tctx $ TypeInstance (typeName t) []
        _ -> throwk $ InternalError
          ("Unexpected missing typedef: " ++ (s_unpack $ showTypePath tp))
          Nothing
    TypeTuple t -> do
      resolvedT <- forM t $ follow ctx tctx
      return $ TypeTuple resolvedT
    TypeTraitConstraint (tp, params) -> do
      resolvedParams <- forM params (follow ctx tctx)
      return $ TypeTraitConstraint (tp, resolvedParams)
    _ -> return t

followType ctx tctx = convertTypeDefinition
  (\_ -> converter return (\_ -> mapType $ follow ctx tctx))
followFunction ctx tctx = convertFunctionDefinition
  (\_ -> converter return (\_ -> mapType $ follow ctx tctx))
followTrait ctx tctx = convertTraitDefinition
  (\_ -> converter return (\_ -> mapType $ follow ctx tctx))
followVariant ctx tctx =
  convertEnumVariant (converter return (\_ -> mapType $ follow ctx tctx))

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
      _ -> return tctx
  UsingImplicit x -> return $ tctx { tctxImplicits = x : tctxImplicits tctx }
  _               -> return tctx

addTypeParams :: TypeContext -> [(TypePath, ConcreteType)] -> TypeContext
addTypeParams tctx params =
  tctx { tctxTypeParams = params ++ tctxTypeParams tctx }

modTypeContext :: CompileContext -> Module -> IO TypeContext
modTypeContext ctx mod = do
  tctx  <- newTypeContext []
  using <- readIORef (modUsing mod)
  foldM (\tctx using -> addUsing ctx tctx using) tctx using

builtinToConcreteType
  :: CompileContext
  -> TypeContext
  -> Module
  -> Str
  -> [TypeSpec]
  -> Span
  -> IO (Maybe ConcreteType)
builtinToConcreteType ctx tctx mod s p pos = do
  case (s, p) of
    -- basics
    ("CString", []) -> return $ Just $ TypePtr $ TypeBasicType $ BasicTypeCChar
    ("Char"   , []) -> return $ Just $ TypeBasicType $ BasicTypeCChar
    ("Int"    , []) -> return $ Just $ TypeBasicType $ BasicTypeCInt
    ("Size"   , []) -> return $ Just $ TypeBasicType $ BasicTypeCSize
    ("Bool"   , []) -> return $ Just $ TypeBasicType $ BasicTypeBool
    ("Int8"   , []) -> return $ Just $ TypeBasicType $ BasicTypeInt 8
    ("Int16"  , []) -> return $ Just $ TypeBasicType $ BasicTypeInt 16
    ("Int32"  , []) -> return $ Just $ TypeBasicType $ BasicTypeInt 32
    ("Int64"  , []) -> return $ Just $ TypeBasicType $ BasicTypeInt 64
    ("Uint8"  , []) -> return $ Just $ TypeBasicType $ BasicTypeUint 8
    ("Uint16" , []) -> return $ Just $ TypeBasicType $ BasicTypeUint 16
    ("Uint32" , []) -> return $ Just $ TypeBasicType $ BasicTypeUint 32
    ("Uint64" , []) -> return $ Just $ TypeBasicType $ BasicTypeUint 64
    ("Float32", []) -> return $ Just $ TypeBasicType $ BasicTypeFloat 32
    ("Float64", []) -> return $ Just $ TypeBasicType $ BasicTypeFloat 64
    ("FILE"   , []) -> return $ Just $ TypeBasicType $ BasicTypeCFile
    -- aliases
    ("Byte"   , []) -> builtinToConcreteType ctx tctx mod "Uint8" [] pos
    ("Short"  , []) -> builtinToConcreteType ctx tctx mod "Int16" [] pos
    ("Long"   , []) -> builtinToConcreteType ctx tctx mod "Int64" [] pos
    ("Uint"   , []) -> builtinToConcreteType ctx tctx mod "Uint32" [] pos
    ("Float"  , []) -> builtinToConcreteType ctx tctx mod "Float32" [] pos
    ("Double" , []) -> builtinToConcreteType ctx tctx mod "Float64" [] pos
    ("Void"   , []) -> return $ Just $ TypeBasicType BasicTypeVoid
    -- compound
    ("Box"    , []) -> throwk $ BasicError
      "Can't infer the trait that a Box should contain; use Box[T] instead"
      (Just pos)
    ("Box", [x]) -> do
      trait <- resolveType ctx tctx mod x
      case trait of
        TypeTraitConstraint (tp, params) -> do
          params <- makeGeneric ctx tp pos params
          return $ Just $ TypeBox tp (map snd params)
        _ -> return Nothing
    ("Ptr", []) -> do
      param <- makeTypeVar ctx pos
      return $ Just $ TypePtr param
    ("Ptr", [x]) -> do
      param <- resolveType ctx tctx mod x
      return $ Just $ TypePtr param
    ("CArray", [x]) -> do
      param <- resolveType ctx tctx mod x
      return $ Just $ TypeArray param Nothing
    ("CArray", [x, size]) -> do
      param <- resolveType ctx tctx mod x
      size  <- resolveType ctx tctx mod size
      return $ Just $ TypeArray param (Just size)
    _ -> return Nothing

getTraitImpl
  :: CompileContext
  -> TypeContext
  -> TraitConstraint
  -> ConcreteType
  -> IO (Maybe (TraitImplementation TypedExpr ConcreteType))
getTraitImpl ctx tctx trait@(traitTp, params) ct = do
  traitDef <- getTraitDefinition ctx traitTp
  ct       <- mapType (follow ctx tctx) ct
  let explicitParams = traitExplicitParams traitDef params
  traitImpls <- h_lookup (ctxImpls ctx) (traitTp, explicitParams)
  result     <- case traitImpls of
    Just x -> do
      lookup <- h_lookup x ct
      case lookup of
        Just y -> return $ Just y
        _      -> return Nothing
    Nothing -> return Nothing
  t <- h_toList (ctxImpls ctx)
  case result of
    Just impl -> return $ Just impl
    Nothing   -> do
      case ct of
        TypeInstance tp params -> do
          def <- getTypeDefinition ctx tp
          case typeSubtype def of
            Abstract { abstractUnderlyingType = u } -> do
              let tctx' = addTypeParams
                    tctx
                    [ (typeSubPath def (paramName param), value)
                    | (param, value) <- zip (typeParams def) params
                    ]
              getTraitImpl ctx tctx' trait u
            _ -> return Nothing
        _ -> return Nothing

functionConcrete f = TypeFunction
  (functionType f)
  [ (argName arg, argType arg) | arg <- functionArgs f ]
  (functionVarargs f)
  []

typeUnresolved ctx tctx ct = do
  t <- mapType (follow ctx tctx) ct
  case t of
    TypeTypeVar _ -> return True
    _             -> return False

getArraySize
  :: CompileContext -> TypeContext -> Maybe Span -> ConcreteType -> IO Int
getArraySize ctx tctx pos s = do
  s <- mapType (follow ctx tctx) s
  case s of
    ConstantType (IntValue i) -> return i
    _ ->
      throwk $ InternalError ("Invalid array size parameter: " ++ show s) pos
