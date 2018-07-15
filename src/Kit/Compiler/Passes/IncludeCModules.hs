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
import Kit.Compiler.TypeContext
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Ir
import Kit.Parser
import Kit.Str

includeCModules :: CompileContext -> IO ()
includeCModules ctx = do
  includes <- readIORef (ctxIncludes ctx)
  forM_ includes (includeCHeader ctx)
  return ()

includeCHeader :: CompileContext -> FilePath -> IO Module
includeCHeader ctx path = do
  existing <- h_lookup (ctxCModules ctx) path
  case existing of
    Just x -> do
      return x
    Nothing -> do
      found <- findSourceFile path (ctxIncludePaths ctx)
      case found of
        Just f -> do
          debugLog ctx $ "found header " ++ show path ++ " at " ++ show f
          mod <- parseCHeader ctx f
          h_insert (ctxCModules ctx) path mod
          return mod
        Nothing ->
          throw
            $ Errs
            $ [ err
                  IncludeError
                  (  "Couldn't find header "
                  ++ show path
                  ++ "; tried searching the following locations: \n\n"
                  ++ (intercalate
                       "\n"
                       [ "  - " ++ (dir </> path)
                       | dir <- ctxIncludePaths ctx
                       ]
                     )
                  )
              ]

parseCHeader :: CompileContext -> FilePath -> IO Module
parseCHeader ctx path = do
  parseResult <- parseCFile (newGCC "gcc")
                            Nothing
                            [ "-I" ++ dir | dir <- ctxIncludePaths ctx ]
                            path
  case parseResult of
    Left e -> throw $ Errs
      [ err IncludeError
            ("Parsing C header " ++ show path ++ " failed: " ++ show e)
      ]
    Right (CTranslUnit decls _) -> do
      mod <- newCMod path
      parseCDecls ctx mod decls
      return mod

parseCDecls :: CompileContext -> Module -> [CExtDecl] -> IO ()
parseCDecls ctx mod [] = do
  return ()
parseCDecls ctx mod (h : t) = do
  case h of
    CDeclExt cdecl -> do
      let declarations        = decomposeCDecl cdecl
      let (_, _, typeSpec, _) = head declarations
      forM_ (parseNonInitSpec typeSpec) (\t -> addTypeDeclaration ctx mod t)
      forM_
        declarations
        (\(name, storageSpec, typeSpec, derivedSpec) -> do
          let t' = (typeFromSpec typeSpec derivedSpec)
          addCDecl ctx mod name t'
          if isTypedef storageSpec
            then do
              forM_ (parseTypedefSpec typeSpec name)
                    (\t -> addTypeDeclaration ctx mod t)
              bindToScope (modTypes mod) name t'
              debugLog ctx
                $  "typedef "
                ++ (s_unpack name)
                ++ " = "
                ++ (show t')
                ++ ""
              return ()
            else return ()
          return ()
        )
      return ()

    _ -> do
      return ()
  parseCDecls ctx mod t

addCDecl :: CompileContext -> Module -> Str -> ConcreteType -> IO ()
addCDecl ctx mod name t = do
  let bindingData = case t of
        TypeFunction t argTypes isVariadic -> FunctionBinding
          t
          [ (name, argType) | (name, argType) <- argTypes ]
          isVariadic
        _ -> VarBinding $ t
  bindToScope (modVars mod) name (newBinding bindingData Nothing)
  debugLog ctx $ "binding " ++ (s_unpack name) ++ ": " ++ (show t) ++ ""
  return ()

-- Parse CDeclSpecs to find the type of a function/variable declaration
parseDeclSpec :: [CTypeSpec] -> Maybe ConcreteType
parseDeclSpec x = _parseDeclSpec x 0 True False
_parseDeclSpec (h : t) width signed float = case h of
  -- simple types; narrow the definition with each specifier
  (CVoidType   _           ) -> Just $ TypeBasicType BasicTypeVoid
  (CBoolType   _           ) -> Just $ TypeBasicType BasicTypeBool
  (CSignedType _           ) -> _parseDeclSpec t width True float
  (CUnsigType  _           ) -> _parseDeclSpec t width False float
  (CFloatType  _           ) -> _parseDeclSpec t 32 signed True
  (CDoubleType _           ) -> _parseDeclSpec t 64 signed True
  (CCharType   _           ) -> _parseDeclSpec t 8 signed False
  (CShortType  _           ) -> _parseDeclSpec t 16 signed False
  (CIntType    _           ) -> _parseDeclSpec t 16 signed False
  (CLongType   _           ) -> _parseDeclSpec t (width + 32) signed False
  (CTypeDef (Ident x _ _) _) -> Just $ TypeTypedef ([], (s_pack x)) []
  -- anonymous structs/enums; TODO: need to generate a stub declaration for these
  (CSUType (CStruct CStructTag (Just (Ident x _ _)) _ _ _) _) ->
    Just $ TypeStruct ([], (s_pack x)) []
  (CEnumType (CEnum (Just (Ident x _ _)) _ _ _) _) ->
    Just $ TypeEnum ([], (s_pack x)) []
  _ -> _parseDeclSpec t width signed float
_parseDeclSpec [] 0     _      _     = Nothing
_parseDeclSpec [] width signed float = Just $ if float
  then TypeBasicType $ BasicTypeFloat width
  else if signed
    then TypeBasicType $ BasicTypeInt width
    else TypeBasicType $ BasicTypeUint width

isTypedef :: [CStorageSpec] -> Bool
isTypedef ((CTypedef _) : _) = True
isTypedef (h            : t) = isTypedef t
isTypedef []                 = False

parseTypedefSpec :: [CTypeSpec] -> Str -> [TypeDefinition Expr (Maybe TypeSpec)]
parseTypedefSpec ((CTypeDef (Ident name _ _) _) : _) _ = [] -- TODO
parseTypedefSpec ((CSUType (CStruct CStructTag _ fields _ _) _) : _) name =
  let fields' = case fields of
        Just f  -> f
        Nothing -> []
  in
    [ (newTypeDefinition name)
        { typeNameMangling = Nothing
        , typeType         = Struct
          { struct_fields = [ newVarDefinition
                                { varName         = structFieldName field
                                , varType         = Just
                                  $ ConcreteType
                                  $ structFieldType field
                                , varNameMangling = Nothing
                                }
                            | field <- fields'
                            ]
          }
        }
    ]
parseTypedefSpec (h : t) name = parseTypedefSpec t name
parseTypedefSpec []      _    = []

parseNonInitSpec ((CSUType (CStruct CStructTag (Just (Ident name _ _)) fields _ _) _) : _)
  = case fields of
    Just fields
      -> [ (newTypeDefinition (s_pack name))
             { typeNameMangling = Nothing
             , typeType         = Struct
               { struct_fields = [ newVarDefinition
                                     { varNameMangling = Nothing
                                     , varName         = structFieldName field
                                     , varType         = Just
                                       $ ConcreteType
                                       $ structFieldType field
                                     }
                                 | field <- fields
                                 ]
               }
             }
         ]
    Nothing -> []
parseNonInitSpec ((CEnumType (CEnum (Just (Ident name _ _)) (Just variants) _ _) _) : _)
  = [ (newTypeDefinition (s_pack name))
        { typeNameMangling = Nothing
        , typeType         = Enum
          { enum_variants = [ newEnumVariant { variantName = s_pack variantName
                                             }
                            | (Ident variantName _ _, _) <- variants
                            ]
          , enum_underlying_type = Nothing
          }
        }
    ]
parseNonInitSpec _ = []

addTypeDeclaration
  :: CompileContext -> Module -> TypeDefinition Expr (Maybe TypeSpec) -> IO ()
addTypeDeclaration ctx mod t = do
  let name = (typeName t)
  tctx <- newTypeContext []
  t'   <- typeDefinitionToConcreteType ctx tctx mod t
  bindToScope (modTypes mod) name t'

structFieldName cdecl =
  let (name, _, _, _) = head (decomposeCDecl cdecl) in name
structFieldType cdecl =
  let (_, _, typeSpec, derivedSpec) = head (decomposeCDecl cdecl)
  in  typeFromSpec typeSpec derivedSpec

parseDerivedTypeSpec :: ConcreteType -> [CDerivedDeclr] -> ConcreteType
parseDerivedTypeSpec ct ((CPtrDeclr _ _) : t) =
  parseDerivedTypeSpec (TypePtr ct) t
parseDerivedTypeSpec ct ((CFunDeclr (Right (params, isVariadic)) _ _) : t) =
  parseDerivedTypeSpec
    (TypeFunction
      ct
      [ (name, typeFromSpec typeSpec derivedSpec)
      | p <- params
      , (name, _, typeSpec, derivedSpec) <- decomposeCDecl p
      , (typeFromSpec typeSpec derivedSpec) /= TypeBasicType BasicTypeVoid
      ]
      isVariadic
    )
    t
parseDerivedTypeSpec ct (h : t) = parseDerivedTypeSpec ct t
parseDerivedTypeSpec ct []      = ct

cDeclrName :: CDeclr -> Str
cDeclrName (CDeclr (Just (Ident x _ _)) _ _ _ _) = s_pack x
cDeclrName (CDeclr Nothing              _ _ _ _) = "_"

cDeclrDerived :: CDeclr -> [CDerivedDeclarator NodeInfo]
cDeclrDerived (CDeclr _ derived _ _ _) = derived

typeFromSpec :: [CTypeSpec] -> [CDerivedDeclr] -> ConcreteType
typeFromSpec typeSpec derivedSpec = parseDerivedTypeSpec
  (case parseDeclSpec typeSpec of
    Just t  -> t
    Nothing -> TypeBasicType BasicTypeUnknown
  )
  (reverse derivedSpec)

type DecomposedCDecl = (Str, [CStorageSpec], [CTypeSpec], [CDerivedDeclr])

decomposeCDecl :: CDecl -> [DecomposedCDecl]
decomposeCDecl (CDecl spec [] _) =
  let (storageSpec, _, _, typeSpec, _, _) = partitionDeclSpecs spec
  in  [("_", storageSpec, typeSpec, [])]
decomposeCDecl (CDecl spec xs _) =
  let (storageSpec, _, _, typeSpec, _, _) = partitionDeclSpecs spec
  in  [ ( case declr of
          Just d  -> cDeclrName d
          Nothing -> "_"
        , storageSpec
        , typeSpec
        , case declr of
          Just d  -> cDeclrDerived d
          Nothing -> []
        )
      | (declr, _, _) <- xs
      ]
