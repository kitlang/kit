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

  resolveType :: CompileContext -> TypeContext -> Module -> TypeSpec -> IO (Maybe ConcreteType)
  resolveType ctx tctx mod t = do
    case t of
      ConcreteType (TypeTypedef t params) -> return $ Just (TypeTypedef t params) -- TODO: dereference immediately here
      ConcreteType ct -> return $ Just ct
      TypeSpec (m, s) params -> do
        case m of
          [] -> do
            scoped <- resolveBinding (tctxTypeParamScopes tctx) s
            case scoped of
              Just x -> return $ Just x
              Nothing -> do
                -- search other modules
                imports <- mapM (getMod ctx) (map fst $ mod_imports mod)
                bound <- resolveBinding (map mod_types (mod : imports)) s
                return $ bound <|> typeNameToConcreteType s
          m -> do
            -- search only a specific module for this type
            imports <- mapM (getMod ctx) [mod' | (mod', _) <- mod_imports mod, mod' == m]
            resolveBinding (map mod_types imports) s
      TypeFunctionSpec rt params args isVariadic -> do
        -- TODO
        return Nothing

  resolveMaybeType :: CompileContext -> TypeContext -> Module -> Maybe TypeSpec -> IO (Maybe ConcreteType)
  resolveMaybeType ctx tctx mod t = do
    case t of
      Just t -> resolveType ctx tctx mod t
      Nothing -> do
        var <- makeTypeVar ctx
        return $ Just var

  resolveTypeOrFail ctx tctx mod pos t = do
    result <- resolveType ctx tctx mod t
    case result of
      Just t -> return t
      Nothing -> throw $ Errs [errp TypingError ("Unknown type: " ++ (show t)) (Just pos)]

  resolveMaybeTypeOrFail ctx tctx mod pos t = do
    result <- resolveMaybeType ctx tctx mod t
    case result of
      Just t -> return t
      Nothing -> throw $ Errs [errp TypingError ("Unknown type: " ++ (show (case t of {Just x -> x}))) (Just pos)]

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
          Just (Left _) -> return t
          -- Hasn't been unified with anything; return type var
          Nothing -> return t
      TypeTypeVar (TypeParamVar s) -> do
        result <- resolveBinding (tctxTypeParamScopes tctx) s
        case result of
          Just t -> return t
          _ -> return t
      t -> return t
