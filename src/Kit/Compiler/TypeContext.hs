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

data DuplicateDeclarationError = DuplicateDeclarationError ModulePath Str Span Span deriving (Eq, Show)
instance Errable DuplicateDeclarationError where
  logError e@(DuplicateDeclarationError mod name pos1 pos2) = do
    logErrorBasic e $ "Duplicate declaration for `" ++ s_unpack name ++ "` in " ++ s_unpack (showModulePath mod) ++ "; \n\nFirst declaration:"
    ePutStrLn "\nSecond declaration:"
    displayFileSnippet pos2
    ePutStrLn "\nFunction, variable, type and trait names must be unique within the same scope."
  errPos (DuplicateDeclarationError _ _ pos _) = Just pos

data TypeContext = TypeContext {
  tctxScopes :: [Scope Binding],
  tctxMacroVars :: [(Str, TypedExpr)],
  tctxRules :: [RuleSet Expr (Maybe TypeSpec)],
  tctxActiveRules :: [(RewriteRule Expr (Maybe TypeSpec), Span)],
  tctxReturnType :: Maybe ConcreteType,
  tctxThis :: Maybe ConcreteType,
  tctxSelf :: Maybe TypePath,
  tctxImplicits :: [TypedExpr],
  tctxTypeParams :: [(Str, ())], -- TODO
  tctxLoopCount :: Int,
  tctxRewriteRecursionDepth :: Int
}

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
    }

unknownType t pos = do
  throw $ KitError $ TypingError ("Unknown type: " ++ (show t)) pos

follow
  :: CompileContext -> TypeContext -> Module -> ConcreteType -> IO ConcreteType
follow ctx tctx mod t = do
  case t of
    TypeInstance tp params -> do
      resolvedParams <- forM params (follow ctx tctx mod)
      return $ TypeInstance tp resolvedParams
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
    TypeEnumConstructor tp d args -> do
      resolvedArgs <- forM
        args
        (\(name, t) -> do
          resolvedArg <- follow ctx tctx mod t
          return (name, resolvedArg)
        )
      return $ TypeEnumConstructor tp d resolvedArgs
    TypeTypedef tp params -> do
      resolveType ctx tctx mod
        $ TypeSpec tp [ ConcreteType p | p <- params ] NoPos
    _ -> return t

resolveModuleBinding
  :: CompileContext -> TypeContext -> Module -> TypePath -> IO (Maybe Binding)
resolveModuleBinding ctx tctx mod (m, name) = do
  importedMods <- getModImports ctx mod
  let searchMods = if null m
        then importedMods
        else (filter (\mod' -> modPath mod' == m) importedMods)
  resolveBinding (map modScope searchMods) name

resolveTypeParam :: Str -> [(Str, ())] -> Maybe Str
resolveTypeParam s (h : t) =
  if fst h == s then Just $ fst h else resolveTypeParam s t
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
    ConcreteType ct            -> follow ctx tctx mod ct
    TupleTypeSpec t pos -> do
      slots <- forM t (resolveType ctx tctx mod)
      return $ TypeTuple slots
    TypeSpec (m, s) params pos -> do
      case m of
        [] -> do
          case (s, tctxSelf tctx) of
            ("Self", Just selfTp) ->
              resolveType ctx tctx mod (TypeSpec selfTp [] (typeSpecPosition t))
            _ -> do
              case resolveTypeParam s (tctxTypeParams tctx) of
                Just x -> return $ TypeTypeParam x
                _      -> do
                  scoped <- resolveBinding (tctxScopes tctx) s
                  case scoped of
                    Just x ->
                      -- named binding exists locally; resolve and return it
                      return $ bindingConcrete x
                    Nothing -> do
                      -- search other modules
                      bound <- resolveBinding (map modScope importedMods) s
                      case bound of
                        Just (Binding { bindingType = TypeBinding _, bindingConcrete = t })
                          -> follow ctx tctx mod t
                        Just (Binding { bindingType = TraitBinding _, bindingConcrete = t })
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
            Just (Binding { bindingType = TypeBinding _, bindingConcrete = t })
              -> follow ctx tctx mod t
            Just (Binding { bindingType = TypedefBinding, bindingConcrete = t })
              -> follow ctx tctx mod t
            _ -> unknownType (s_unpack $ showTypePath (m, s)) pos

    FunctionTypeSpec rt params args isVariadic -> do
      -- TODO
      unknownType "TODO" NoPos

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
    TypeTypeVar x -> do
      info <- getTypeVar ctx x
      case typeVarValue info of
        -- Specific known type
        Just t  -> knownType ctx tctx mod t
        -- No specific type; return type var
        Nothing -> follow ctx tctx mod t
    t -> follow ctx tctx mod t

addUsing
  :: CompileContext
  -> TypeContext
  -> Module
  -> UsingType TypedExpr ConcreteType
  -> IO TypeContext
addUsing ctx tctx mod using = case using of
  UsingRuleSet (TypeRuleSet (modPath, n)) -> do
    defMod <- getMod ctx modPath
    def <- resolveLocal (modScope defMod) n
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
      Just (Binding { bindingType = RuleSetBinding r }) -> do
        return $ tctx { tctxRules = r : tctxRules tctx }
      _ -> return tctx
  UsingImplicit x -> return $ tctx { tctxImplicits = x : tctxImplicits tctx }
  _ -> throwk $ InternalError ("Unexpected using clause: " ++ show using) Nothing

modTypeContext :: CompileContext -> Module -> IO TypeContext
modTypeContext ctx mod = do
  imports <- getModImports ctx mod
  tctx    <- newTypeContext (map modScope imports)
  using   <- readIORef (modUsing mod)
  foldM (\tctx using -> addUsing ctx tctx mod using) tctx using

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
    ("Long"   , [] ) -> builtinToConcreteType ctx tctx mod "Int64" []
    ("Uint"   , [] ) -> builtinToConcreteType ctx tctx mod "Uint32" []
    ("Float"  , [] ) -> builtinToConcreteType ctx tctx mod "Float32" []
    ("Double" , [] ) -> builtinToConcreteType ctx tctx mod "Float64" []
    ("Void"   , [] ) -> return $ Just $ TypeBasicType BasicTypeVoid
    -- compound
    ("Ptr"    , [x]) -> do
      param <- resolveType ctx tctx mod x
      return $ Just $ TypePtr param
    ("CArray", [x]) -> do
      param <- resolveType ctx tctx mod x
      return $ Just $ TypeArr param Nothing
    _ -> return Nothing
