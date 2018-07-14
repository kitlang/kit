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
  throw $ Errs [errp TypingError ("Unknown type: " ++ (show t)) (Just pos)]

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

{-
  Attempt to resolve a TypeSpec into a ConcreteType; fail if it isn't a known
  type.
-}
resolveType
  :: CompileContext -> TypeContext -> Module -> TypeSpec -> IO ConcreteType
resolveType ctx tctx mod t = do
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
              imports  <- mapM (getMod ctx) (map fst $ modImports mod)
              includes <- mapM (getCMod ctx) (map fst $ modIncludes mod)
              bound    <- resolveBinding
                (map modTypes (mod : (imports ++ includes)))
                s
              case bound of
                Just t  -> follow ctx tctx mod t
                Nothing -> do
                  builtin <- builtinToConcreteType ctx tctx mod s params
                  case builtin of
                    Just t  -> follow ctx tctx mod t
                    Nothing -> unknownType s pos
        m -> do
          -- search only a specific module for this type
          imports <- mapM (getMod ctx)
                          [ mod' | (mod', _) <- modImports mod, mod' == m ]
          result <- resolveBinding (map modTypes imports) s
          case result of
            Just t  -> follow ctx tctx mod t
            Nothing -> unknownType s pos

    TypeFunctionSpec rt params args isVariadic -> do
      -- TODO
      unknownType "???" null_span

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
        Just t -> knownType ctx tctx mod t
        -- No specific type; return type var
        Nothing -> follow ctx tctx mod t
    TypeTypeVar (TypeParamVar s) -> do
      result <- resolveBinding (tctxTypeParamScopes tctx) s
      case result of
        Just t -> follow ctx tctx mod t
        _      -> follow ctx tctx mod t
    t -> follow ctx tctx mod t


typeDefinitionToConcreteType
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypeDefinition Expr (Maybe TypeSpec)
  -> IO ConcreteType
typeDefinitionToConcreteType ctx tctx mod (TypeDefinition { typeName = name, typeParams = params, typeType = t })
  = do
    resolvedParams <- mapM (resolveType ctx tctx mod)
                           (map typeParamToSpec params)
    case t of
      Atom       -> return $ TypeAtom name
      Struct{}   -> return $ TypeStruct tp resolvedParams
      Enum{}     -> return $ TypeEnum tp resolvedParams
      Abstract{} -> return $ TypeAbstract tp resolvedParams
  where tp = (modPath mod, name)

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
    ("CString", [] ) -> return $ Just $ TypeBasicType $ CPtr $ BasicTypeInt 8
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


typeNameToConcreteType :: Str -> Maybe ConcreteType
typeNameToConcreteType "CString" = Just $ TypeBasicType $ CPtr $ BasicTypeInt 8

typeNameToConcreteType _         = Nothing
