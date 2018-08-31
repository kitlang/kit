module Kit.Compiler.TypeContext where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.IORef
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypedExpr
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser
import Kit.Str

data TypingError = TypingError String Span deriving (Eq, Show)
instance Errable TypingError where
  logError e@(TypingError s _) = logErrorBasic (KitError e) s
  errPos (TypingError _ pos) = Just pos

data TypeContext = TypeContext {
  tctxScopes :: [Scope Binding],
  tctxMacroVars :: [(Str, TypedExpr)],
  tctxRules :: [RuleSet Expr (Maybe TypeSpec)],
  tctxActiveRules :: [(RewriteRule Expr (Maybe TypeSpec), Span)],
  tctxReturnType :: Maybe ConcreteType,
  tctxThis :: Maybe ConcreteType,
  tctxSelf :: Maybe ConcreteType,
  tctxImplicits :: [TypedExpr],
  tctxTypeParams :: [(TypePath, ConcreteType)],
  tctxLoopCount :: Int,
  tctxRewriteRecursionDepth :: Int,
  tctxState :: TypeContextState, tctxTemps :: Maybe (IORef [TypedExpr])
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
    , tctxTemps                 = Nothing
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
    ConcreteType ct     -> follow ctx tctx ct
    TupleTypeSpec t pos -> do
      slots <- forM t (resolveType ctx tctx mod)
      return $ TypeTuple slots
    TypeSpec (m, s) params pos -> do
      resolvedParams <- forM params (resolveType ctx tctx mod)
      case m of
        [] -> do
          case (s, tctxSelf tctx) of
            ("Self", Just self) -> return self
            _                   -> do
              case resolveTypeParam ([], s) (tctxTypeParams tctx) of
                Just x -> do
                  return x
                _ -> do
                  scoped <- resolveBinding (tctxScopes tctx) s
                  ct     <- case scoped of
                    Just (TypeBinding t) ->
                      return $ TypeInstance (typeName t) resolvedParams
                    Just (TraitBinding t) ->
                      return $ TypeTraitConstraint (traitName t, resolvedParams)
                    Just (VarBinding v) -> return $ varType v
                    Just (RuleSetBinding r) ->
                      return $ TypeRuleSet (ruleSetName r)
                    _ -> do
                      -- search other modules
                      bound <- resolveBinding (map modScope importedMods) s
                      case bound of
                        Just (TypeBinding t) ->
                          return $ TypeInstance (typeName t) resolvedParams
                        Just (TraitBinding t) -> return
                          $ TypeTraitConstraint (traitName t, resolvedParams)
                        _ -> do
                          builtin <- builtinToConcreteType ctx
                                                           tctx
                                                           mod
                                                           s
                                                           params
                                                           pos
                          case builtin of
                            Just t  -> follow ctx tctx t
                            Nothing -> unknownType s pos
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
          result <- resolveBinding (map modScope importedMods) s
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
  case t of
    TypeSelf -> do
      case tctxSelf tctx of
        Just x  -> follow ctx tctx x
        Nothing -> return TypeSelf
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
    TypeArr t len -> do
      resolved <- follow ctx tctx t
      return $ TypeArr resolved len
    TypeEnumConstructor tp d args params -> do
      resolvedArgs <- forM
        args
        (\(name, t) -> do
          resolvedArg <- follow ctx tctx t
          return (name, resolvedArg)
        )
      resolvedParams <- forM params (follow ctx tctx)
      return $ TypeEnumConstructor tp d resolvedArgs resolvedParams
    TypeTypedef tp@(modPath, name) params -> do
      defMod  <- getMod ctx modPath
      binding <- resolveLocal (modScope defMod) name
      case binding of
        Just (TypedefBinding t _) -> follow ctx tctx t
        _                         -> throwk $ InternalError
          ("Unexpected missing typedef: " ++ (s_unpack $ showTypePath tp))
          Nothing
    TypeTuple t -> do
      resolvedT <- forM t $ follow ctx tctx
      return $ TypeTuple resolvedT
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
  UsingRuleSet (TypeRuleSet (modPath, n)) -> do
    defMod <- getMod ctx modPath
    def    <- resolveLocal (modScope defMod) n
    case def of
      -- Just (DeclType t) -> do
      --   -- FIXME: this is terrible...
      --   binding <- scopeGet (modScope mod) n
      --   return $ tctx
      --     { tctxRules = ((typeRuleSet t)
      --                     { ruleSetThis = Just (bindingConcrete binding)
      --                     }
      --                   )
      --       : tctxRules tctx
      --     }
      Just (RuleSetBinding r) -> do
        return $ tctx { tctxRules = r : tctxRules tctx }
      _ -> return tctx
  UsingImplicit x -> return $ tctx { tctxImplicits = x : tctxImplicits tctx }
  _ ->
    throwk $ InternalError ("Unexpected using clause: " ++ show using) Nothing

addTypeParams :: TypeContext -> [(TypePath, ConcreteType)] -> TypeContext
addTypeParams tctx params =
  tctx { tctxTypeParams = params ++ tctxTypeParams tctx }

modTypeContext :: CompileContext -> Module -> IO TypeContext
modTypeContext ctx mod = do
  imports <- getModImports ctx mod
  tctx    <- newTypeContext (map modScope imports)
  using   <- readIORef (modUsing mod)
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
      "Can't infer the trait that a generic Box should contain; use Box[T] instead"
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
    ("CArray", []) -> do
      param <- makeTypeVar ctx pos
      return $ Just $ TypeArr param Nothing
    ("CArray", [x]) -> do
      param <- resolveType ctx tctx mod x
      return $ Just $ TypeArr param Nothing
    _ -> return Nothing

getTraitImpl
  :: CompileContext
  -> TypeContext
  -> TraitConstraint
  -> ConcreteType
  -> IO (Maybe (TraitImplementation TypedExpr ConcreteType))
getTraitImpl ctx tctx trait ct = do
  ct         <- follow ctx tctx ct
  traitImpls <- h_lookup (ctxImpls ctx) trait
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
        TypeInstance (modPath, name) params -> do
          def <- getTypeDefinition ctx modPath name
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
