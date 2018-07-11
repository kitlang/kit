module Kit.Compiler.TypeContext where

  import Control.Applicative
  import Control.Exception
  import Control.Monad
  import Kit.Ast
  import Kit.Compiler.Context
  import Kit.Compiler.Module
  import Kit.Compiler.Scope
  import Kit.Error
  import Kit.HashTable
  import Kit.Parser
  import Kit.Str

  data TypeContext = TypeContext {
    tctxScopes :: [Scope Binding],
    tctxTypeParamScopes :: [Scope ConcreteType],
    tctxReturnType :: Maybe ConcreteType,
    tctxThis :: Maybe ConcreteType,
    tctxSelf :: Maybe ConcreteType,
    tctxLoopCount :: Int,
    tctxRewriteRecursionDepth :: Int
  }

  newTypeContext :: [Scope Binding] -> IO TypeContext
  newTypeContext scopes = do
    scope <- newScope
    return $ TypeContext {
      tctxScopes = scope : scopes,
      tctxTypeParamScopes = [],
      tctxReturnType = Nothing,
      tctxThis = Nothing,
      tctxSelf = Nothing,
      tctxLoopCount = 0,
      tctxRewriteRecursionDepth = 0
    }

  -- TODO: position information
  unknownType t pos = do throw $ Errs [errp TypingError ("Unknown type: " ++ (show t)) (Just pos)]

  follow :: CompileContext -> TypeContext -> ConcreteType -> IO ConcreteType
  follow ctx tctx t = do
    case t of
      TypeStruct tp params -> do
        resolvedParams <- forM params (follow ctx tctx)
        return $ TypeStruct tp resolvedParams
      TypeEnum tp params -> do
        resolvedParams <- forM params (follow ctx tctx)
        return $ TypeEnum tp resolvedParams
      TypeAbstract tp params -> do
        resolvedParams <- forM params (follow ctx tctx)
        return $ TypeAbstract tp resolvedParams
      TypeFunction t args varargs -> do
        resolved <- follow ctx tctx t
        resolvedArgs <- forM args (\(name, t) -> do resolvedArg <- follow ctx tctx t; return (name, resolvedArg))
        return $ TypeFunction resolved resolvedArgs varargs
      TypePtr t -> do
        resolved <- follow ctx tctx t
        return $ TypePtr resolved
      TypeArr t len -> do
        resolved <- follow ctx tctx t
        return $ TypeArr resolved len
      TypeEnumConstructor tp args -> do
        resolvedArgs <- forM args (\(name, t) -> do resolvedArg <- follow ctx tctx t; return (name, resolvedArg))
        return $ TypeEnumConstructor tp resolvedArgs
      _ -> return t

  resolveType :: CompileContext -> TypeContext -> Module -> TypeSpec -> IO ConcreteType
  resolveType ctx tctx mod t = do
    case t of
      ConcreteType ct -> follow ctx tctx ct
      TypeSpec (m, s) params pos -> do
        case m of
          [] -> do
            scoped <- resolveBinding (tctxTypeParamScopes tctx) s
            case scoped of
              Just x ->
                -- named binding exists locally; resolve and return it
                resolveType ctx tctx mod (ConcreteType x)
              Nothing -> do
                -- search other modules
                imports <- mapM (getMod ctx) (map fst $ mod_imports mod)
                includes <- mapM (getCMod ctx) (map fst $ mod_includes mod)
                bound <- resolveBinding (map mod_types (mod : (imports ++ includes))) s
                case (bound <|> typeNameToConcreteType s) of
                  Just t -> return t
                  Nothing -> unknownType s pos
          m -> do
            -- search only a specific module for this type
            imports <- mapM (getMod ctx) [mod' | (mod', _) <- mod_imports mod, mod' == m]
            result <- resolveBinding (map mod_types imports) s
            case result of
              Just t -> return t
              Nothing -> unknownType s pos

      TypeFunctionSpec rt params args isVariadic -> do
        -- TODO
        unknownType "???" null_span

  resolveMaybeType :: CompileContext -> TypeContext -> Module -> Maybe TypeSpec -> IO ConcreteType
  resolveMaybeType ctx tctx mod t = do
    case t of
      Just t -> resolveType ctx tctx mod t
      Nothing -> makeTypeVar ctx

  resolveEnum :: CompileContext -> TypeContext -> Module -> Str -> IO (Maybe EnumConstructor)
  resolveEnum ctx tctx mod s = do
    imports <- mapM (getMod ctx) (map fst $ mod_imports mod)
    resolveBinding (map mod_enums (mod : imports)) s

  knownType :: CompileContext -> TypeContext -> ConcreteType -> IO ConcreteType
  knownType ctx tctx t = do
    case t of
      TypeTypeVar (TypeVar x) -> do
        result <- h_lookup (ctxTypeVariables ctx) (x)
        case result of
          -- Specific known type
          Just (Right t) -> knownType ctx tctx t
          -- Constraints, but no specific type; return type var
          Just (Left _) -> follow ctx tctx t
          -- Hasn't been unified with anything; return type var
          Nothing -> follow ctx tctx t
      TypeTypeVar (TypeParamVar s) -> do
        result <- resolveBinding (tctxTypeParamScopes tctx) s
        case result of
          Just t -> follow ctx tctx t
          _ -> follow ctx tctx t
      t -> follow ctx tctx t
