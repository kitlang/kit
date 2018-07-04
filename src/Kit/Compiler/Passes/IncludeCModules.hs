module Kit.Compiler.Passes.IncludeCModules where

  import Control.Exception
  import Control.Monad
  import Data.IORef
  import Data.List
  import System.Directory
  import System.FilePath
  import Language.C
  import Language.C.Data.Ident
  import Language.C.System.GCC
  import Kit.Ast
  import Kit.Compiler.Context
  import Kit.Compiler.Module
  import Kit.Compiler.Scope
  import Kit.Compiler.TypeUsage
  import Kit.Compiler.Utils
  import Kit.Error
  import Kit.HashTable
  import Kit.Ir
  import Kit.Parser
  import Kit.Str

  includeCModules :: CompileContext -> IO ()
  includeCModules ctx = do
    includes <- readIORef (context_includes ctx)
    forM includes (includeCHeader ctx)
    return ()

  includeCHeader :: CompileContext -> FilePath -> IO Module
  includeCHeader ctx path = do
    existing <- h_lookup (context_cmodules ctx) path
    case existing of
      Just x -> do return x
      Nothing -> do
        debugLog ctx $ "searching for header " ++ show path
        found <- findSourceFile path (context_include_paths ctx)
        case found of
          Just f -> do
            mod <- parseCHeader ctx f
            h_insert (context_cmodules ctx) path mod
            return mod
          Nothing -> throw $ Errs $ [
              err IncludeError ("Couldn't find header " ++ show path ++
                                "; tried searching the following locations: \n\n" ++
                                (intercalate "\n" ["  - " ++ (dir </> path) | dir <- context_include_paths ctx]))
            ]

  parseCHeader :: CompileContext -> FilePath -> IO Module
  parseCHeader ctx path = do
    putStrLn path
    parseResult <- parseCFile (newGCC "gcc") Nothing [] path
    case parseResult of
      Left e -> throw $ Errs [err IncludeError ("Parsing C header " ++ show path ++ " failed: " ++ show e)]
      Right (CTranslUnit decls _) -> do
        mod <- newCMod []
        parseCDecls ctx mod decls
        return mod

  parseCDecls :: CompileContext -> Module -> [CExtDecl] -> IO ()
  parseCDecls ctx mod [] = do return ()
  parseCDecls ctx mod (h:t) = do
    case h of
      CDeclExt (CDecl spec init _) -> do
        case parseDeclSpec spec of
          Just t -> do
            forM (map (\(decl, _, _) -> decl) init) (addCDecl ctx mod t)
            return ()
          Nothing -> do
            forM init (\(decl, _, _) -> do
              result <- parseTypedefSpec spec decl
              case result of
                Just t -> addTypeDeclaration ctx mod t
                Nothing -> do return ())
            return ()
      {-CFDefExt f -> do
        -- TODO
        putStrLn $ show h-}
      _ -> do return ()
    parseCDecls ctx mod t

  addCDecl :: CompileContext -> Module -> ConcreteType -> Maybe CDeclr -> IO ()
  addCDecl ctx mod t (Just (CDeclr (Just (Ident name _ _)) (derived) _ _ _)) = do
    putStrLn $ "add c decl: " ++ name ++ " " ++ (show derived) ++ " " ++ (show t)
    return ()
  addCDecl ctx mod t _ = do
    return ()

  -- Parse CDeclSpecs to find the type of a function/variable declaration
  parseDeclSpec :: [CDeclSpec] -> Maybe ConcreteType
  parseDeclSpec x = _parseDeclSpec x 0 True False
  _parseDeclSpec (h:t) width signed float = case h of
    -- simple types; narrow the definition with each specifier
    CTypeSpec (CVoidType _) -> Just $ TypeBasicType BasicTypeVoid
    CTypeSpec (CBoolType _) -> Just $ TypeBasicType BasicTypeBool
    CTypeSpec (CSignedType _) -> _parseDeclSpec t width True float
    CTypeSpec (CUnsigType _) -> _parseDeclSpec t width False float
    CTypeSpec (CFloatType _) -> _parseDeclSpec t 32 signed True
    CTypeSpec (CDoubleType _) -> _parseDeclSpec t 64 signed True
    CTypeSpec (CCharType _) -> _parseDeclSpec t 8 signed False
    CTypeSpec (CShortType _) -> _parseDeclSpec t 16 signed False
    CTypeSpec (CIntType _) -> _parseDeclSpec t 16 signed False
    CTypeSpec (CLongType _) -> _parseDeclSpec t (width + 32) signed False
    CTypeSpec (CTypeDef (Ident x _ _) _) -> Just $ TypeTypedef [] (s_pack x)
    -- anonymous structs/enums; TODO: need to generate a stub declaration for these
    CTypeSpec (CSUType (CStruct CStructTag (Just (Ident x _ _)) _ _ _) _) -> Just $ TypeStruct [] (s_pack x)
    CTypeSpec (CEnumType (CEnum (Just (Ident x _ _)) _ _ _) _) -> Just $ TypeEnum [] (s_pack x)
    -- this is a typedef; we'll handle it separately
    CStorageSpec (CTypedef _) -> Nothing

    _ -> _parseDeclSpec t width signed float
  _parseDeclSpec [] 0 _ _ = Nothing
  _parseDeclSpec [] width signed float = Just $
    if float then TypeBasicType $ BasicTypeFloat width
    else if signed then TypeBasicType $ BasicTypeInt width
    else TypeBasicType $ BasicTypeUint width

  parseTypedefSpec :: [CDeclSpec] -> Maybe CDeclr -> IO (Maybe TypeDefinition)
  parseTypedefSpec ((CStorageSpec (CTypedef _)):(CTypeSpec (CTypeDef (Ident name _ _) _)):_) (Just decl) = do
    return Nothing
  parseTypedefSpec a@((CStorageSpec (CTypedef _)):b@(CTypeSpec (CSUType (CStruct CStructTag (Just (Ident name _ _)) fields _ _) _)):_) (Just (CDeclr (Just (Ident typedefName _ _)) _ _ _ _)) = do
    let fields' = case fields of
                    Just f -> f
                    Nothing -> []
    return $ Just (newTypeDefinition (s_pack typedefName)) {
      type_type = Struct {
        struct_fields = [newVarDefinition {var_name = Var (structFieldName field), var_type = structFieldType field} | field <- fields']
      }
    }
  parseTypedefSpec a b  = do return Nothing

  addTypeDeclaration :: CompileContext -> Module -> TypeDefinition -> IO ()
  addTypeDeclaration ctx mod t = do
    let name = (type_name t)
    usage <- newTypeUsage t
    bindToScope (mod_types mod) name usage

  structFieldName (CDecl _ [(Just (CDeclr (Just (Ident name _ _)) _ _ _ _), _, _)] _) = s_pack name
  structFieldType (CDecl t [(Just (CDeclr _ _ _ _ _), _, _)] _) =
    case parseDeclSpec t of
      Just t -> Just (ConcreteType t)
      Nothing -> Nothing
