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

data TypingError = TypingError String Span deriving (Eq, Show)
instance Errable TypingError where
  logError e@(TypingError s _) = logErrorBasic (KitError e) s
  errPos (TypingError _ pos) = Just pos

data DuplicateDeclarationError = DuplicateDeclarationError ModulePath Str Span Span deriving (Eq, Show)
instance Errable DuplicateDeclarationError where
  logError e@(DuplicateDeclarationError mod name pos1 pos2) = do
    logErrorBasic e $ "Duplicate declaration for `" ++ s_unpack name ++ "` in " ++ s_unpack (showModulePath mod) ++ "; \n\nFirst declaration:"
    ePutStrLn "\nSecond declaration:"
    case file pos2 of
      Just fp -> displayFileSnippet (s_unpack fp) pos2
      _ -> return ()
    ePutStrLn "\nFunction, variable, type and trait names must be unique within the same scope."
  errPos (DuplicateDeclarationError _ _ pos _) = Just pos

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
  return $ TypeContext
    { tctxScopes                = scope : scopes
    , tctxTypeParamScopes       = []
    , tctxReturnType            = Nothing
    , tctxThis                  = Nothing
    , tctxSelf                  = Nothing
    , tctxLoopCount             = 0
    , tctxRewriteRecursionDepth = 0
    }

-- TODO: position information
unknownType t pos = do
  throw $ KitError $ TypingError ("Unknown type: " ++ (show t)) pos

follow
  :: CompileContext -> TypeContext -> Module -> ConcreteType -> IO ConcreteType
follow ctx tctx mod t = do
  case t of
    TypeStruct tp params -> do
      resolvedParams <- forM params (follow ctx tctx mod)
      return $ TypeStruct tp resolvedParams
    TypeEnum tp params -> do
      resolvedParams <- forM params (follow ctx tctx mod)
      return $ TypeEnum tp resolvedParams
    TypeAbstract tp params -> do
      resolvedParams <- forM params (follow ctx tctx mod)
      return $ TypeAbstract tp resolvedParams
    TypeFunction t args varargs -> do
      resolved     <- follow ctx tctx mod t
      resolvedArgs <- forM
        args
        (\(name, t) -> do
          resolvedArg <- follow ctx tctx mod t
          return (name, resolvedArg)
        )
      return $ TypeFunction resolved resolvedArgs varargs
    TypePtr t -> do
      resolved <- follow ctx tctx mod t
      return $ TypePtr resolved
    TypeArr t len -> do
      resolved <- follow ctx tctx mod t
      return $ TypeArr resolved len
    TypeEnumConstructor tp args -> do
      resolvedArgs <- forM
        args
        (\(name, t) -> do
          resolvedArg <- follow ctx tctx mod t
          return (name, resolvedArg)
        )
      return $ TypeEnumConstructor tp resolvedArgs
    TypeTypedef tp params -> do
      resolveType ctx tctx mod
        $ TypeSpec tp [ ConcreteType p | p <- params ] null_span
    _ -> return t

resolveModuleBinding
  :: CompileContext -> TypeContext -> Module -> TypePath -> IO (Maybe Binding)
resolveModuleBinding ctx tctx mod (m, name) = do
  importedMods <- getModImports ctx mod
  let searchMods = if null m
        then importedMods
        else (filter (\mod' -> modPath mod' == m) importedMods)
  resolveBinding (map modScope searchMods) name

{-
  Attempt to resolve a TypeSpec into a ConcreteType; fail if it isn't a known
  type.
-}
resolveType
  :: CompileContext -> TypeContext -> Module -> TypeSpec -> IO ConcreteType
resolveType ctx tctx mod t = do
  importedMods <- getModImports ctx mod
  case t of
    ConcreteType ct            -> follow ctx tctx mod ct
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
              bound <- resolveBinding (map modScope importedMods) s
              case bound of
                Just (Binding { bindingType = TypeBinding, bindingConcrete = t })
                  -> follow ctx tctx mod t
                _ -> do
                  builtin <- builtinToConcreteType ctx tctx mod s params
                  case builtin of
                    Just t  -> follow ctx tctx mod t
                    Nothing -> unknownType s pos
        m -> do
          -- search only a specific module for this type
          result <- resolveBinding (map modScope importedMods) s
          case result of
            Just (Binding { bindingType = TypeBinding, bindingConcrete = t })
              -> follow ctx tctx mod t
            _ -> unknownType s pos

    TypeFunctionSpec rt params args isVariadic -> do
      -- TODO
      unknownType "TODO" null_span

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

knownType
  :: CompileContext -> TypeContext -> Module -> ConcreteType -> IO ConcreteType
knownType ctx tctx mod t = do
  case t of
    TypeTypeVar (TypeVar x) -> do
      info <- h_get (ctxTypeVariables ctx) (x)
      case typeVarValue info of
        -- Specific known type
        Just t  -> knownType ctx tctx mod t
        -- No specific type; return type var
        Nothing -> follow ctx tctx mod t
    TypeTypeVar (TypeParamVar s) -> do
      result <- resolveBinding (tctxTypeParamScopes tctx) s
      case result of
        Just t -> follow ctx tctx mod t
        _      -> follow ctx tctx mod t
    t -> follow ctx tctx mod t

builtinToConcreteType
  :: CompileContext
  -> TypeContext
  -> Module
  -> Str
  -> [TypeSpec]
  -> IO (Maybe ConcreteType)
builtinToConcreteType ctx tctx mod s p = do
  case (s, p) of
    -- basics
    ("CString", [] ) -> return $ Just $ TypePtr $ TypeBasicType $ BasicTypeInt 8
    ("Bool"   , [] ) -> return $ Just $ TypeBasicType $ BasicTypeBool
    ("Int8"   , [] ) -> return $ Just $ TypeBasicType $ BasicTypeInt 8
    ("Int16"  , [] ) -> return $ Just $ TypeBasicType $ BasicTypeInt 16
    ("Int32"  , [] ) -> return $ Just $ TypeBasicType $ BasicTypeInt 32
    ("Int64"  , [] ) -> return $ Just $ TypeBasicType $ BasicTypeInt 64
    ("Uint8"  , [] ) -> return $ Just $ TypeBasicType $ BasicTypeUint 8
    ("Uint16" , [] ) -> return $ Just $ TypeBasicType $ BasicTypeUint 16
    ("Uint32" , [] ) -> return $ Just $ TypeBasicType $ BasicTypeUint 32
    ("Uint64" , [] ) -> return $ Just $ TypeBasicType $ BasicTypeUint 64
    ("Float32", [] ) -> return $ Just $ TypeBasicType $ BasicTypeFloat 32
    ("Float64", [] ) -> return $ Just $ TypeBasicType $ BasicTypeFloat 64
    -- aliases
    ("Byte"   , [] ) -> builtinToConcreteType ctx tctx mod "Uint8" []
    ("Char"   , [] ) -> builtinToConcreteType ctx tctx mod "Int8" []
    ("Short"  , [] ) -> builtinToConcreteType ctx tctx mod "Int16" []
    ("Int"    , [] ) -> builtinToConcreteType ctx tctx mod "Int32" []
    ("Uint"   , [] ) -> builtinToConcreteType ctx tctx mod "Uint32" []
    ("Long"   , [] ) -> builtinToConcreteType ctx tctx mod "Int64" []
    ("Float"  , [] ) -> builtinToConcreteType ctx tctx mod "Float32" []
    ("Double" , [] ) -> builtinToConcreteType ctx tctx mod "Float64" []
    ("Void"   , [] ) -> return $ Just $ TypeBasicType BasicTypeVoid
    -- compound
    ("Ptr"    , [x]) -> do
      param <- resolveType ctx tctx mod x
      return $ Just $ TypePtr param
    ("Arr", [x]) -> do
      param <- resolveType ctx tctx mod x
      return $ Just $ TypeArr param Nothing
    _ -> return Nothing
