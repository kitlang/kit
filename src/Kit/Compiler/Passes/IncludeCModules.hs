module Kit.Compiler.Passes.IncludeCModules where

import Control.Monad
import Data.IORef
import Data.List
import System.FilePath
import Language.C
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.System.GCC
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypedExpr
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser
import Kit.Str

data IncludeError = IncludeError FilePath [FilePath] deriving (Eq, Show)
instance Errable IncludeError where
  logError e@(IncludeError fp searchPaths) =
    logErrorBasic e $ (  "Couldn't find C header <"
                                ++ fp
                                ++ ">; tried searching the following locations: \n\n"
                                ++ (intercalate
                                    "\n"
                                    [ "  - " ++ s | s <- searchPaths]
                                  )
                                )

{-
  For each C header discovered during the BuildModuleGraph pass, parse the
  header to discover all declarations, and make these available from Kit.
-}
includeCModules :: CompileContext -> IO ()
includeCModules ctx = do
  includes <- readIORef (ctxIncludes ctx)
  existing <- h_lookup (ctxModules ctx) externModPath
  mod      <- case existing of
    Just x  -> return x
    Nothing -> do
      mod <- newCMod
      h_insert (ctxModules ctx) externModPath mod
      return mod
  forM_ (nub includes) $ includeCHeader ctx mod
  return ()

includeCHeader :: CompileContext -> Module -> FilePath -> IO ()
includeCHeader ctx mod path = do
  found <- findSourceFile path (ctxIncludePaths ctx)
  case found of
    Just f -> do
      debugLog ctx $ "found header " ++ show path ++ " at " ++ show f
      parseCHeader ctx mod f
    Nothing -> throwk
      $ IncludeError path [ (dir </> path) | dir <- ctxIncludePaths ctx ]

parseCHeader :: CompileContext -> Module -> FilePath -> IO ()
parseCHeader ctx mod path = do
  compiler <- findCompiler ctx
  debugLog ctx ("invoking C preprocessor at " ++ compiler)
  parseResult <- parseCFile
    (newGCC compiler)
    Nothing
    -- TODO: defines
    (  [ "-I" ++ dir | dir <- ctxIncludePaths ctx ]
    ++ (defaultCompileArgs ctx $ takeFileName compiler)
    )
    path
  case parseResult of
    Left e -> throwk $ BasicError
      ("Parsing C header " ++ show path ++ " failed: " ++ show e)
      Nothing
    Right (CTranslUnit decls _) -> do
      parseCDecls ctx mod path decls

unknownTypeWarning :: CompileContext -> Module -> Str -> Span -> IO ()
unknownTypeWarning ctx mod name pos = do
  warningLog
    $  "couldn't determine type of "
    ++ (s_unpack name)
    ++ " in "
    ++ (show mod)
    ++ " "
    ++ (show pos)
    ++ "; attempts to access values of this type will fail"
  displayFileSnippet pos

parseCDecls :: CompileContext -> Module -> FilePath -> [CExtDecl] -> IO ()
parseCDecls ctx mod path [] = do
  return ()
parseCDecls ctx mod path (h : t) = do
  case h of
    CDeclExt cdecl -> do
      let ann     = annotation cdecl
      let nodePos = posOfNode ann
      let pos = if isSourcePos nodePos
            then sp (posFile nodePos)
                    (posRow nodePos)
                    (posColumn nodePos)
                    (posRow nodePos)
                    (posColumn nodePos)
            else NoPos
      let (storageSpec, typeSpec, initializers) = decomposeCDecl cdecl
      if isTypedef storageSpec
        then do
          -- this is a typedef
          forM_ initializers (defineTypedef ctx mod typeSpec pos)
          -- if there's a named struct/enum/union, even in a typedef, define it here
          defineNamedStructsEnumsUnions ctx mod pos typeSpec
        else if null initializers
          then -- this is a non-typedef struct, enum or union declaration
               defineNamedStructsEnumsUnions ctx mod pos typeSpec
          else -- this is one or more variable/function declarations
               forM_
            (initializers)
            (\(name, declr) -> do
              let t' = parseType (modPath mod) typeSpec (reverse declr)
              case t' of
                TypeBasicType BasicTypeUnknown ->
                  unknownTypeWarning ctx mod name pos
                _ -> return ()
              veryNoisyDebugLog ctx
                $  "bind "
                ++ (s_unpack name)
                ++ ": "
                ++ (show t')
              addCDecl ctx mod name t' pos
            )
    _ -> do
      return ()
  parseCDecls ctx mod path t

defineTypedef
  :: CompileContext
  -> Module
  -> [CTypeSpec]
  -> Span
  -> (Str, [CDerivedDeclr])
  -> IO ()
defineTypedef ctx mod typeSpec pos (name, declr) = do
  let t' = parseType (modPath mod) typeSpec declr
  veryNoisyDebugLog ctx $ "typedef " ++ (s_unpack name) ++ ": " ++ (show t')
  when (t' == TypeBasicType BasicTypeUnknown)
    $ unknownTypeWarning ctx mod name pos
  h_insert (ctxTypedefs ctx) name t'

parseType :: ModulePath -> [CTypeSpec] -> [CDerivedDeclr] -> ConcreteType
parseType m typeSpec declr =
  parseDerivedType m declr (parseDeclSpec m typeSpec)
parseDerivedType m (h' : t') ct =
  let p = parseDerivedType m t'
  in
    case h' of
      (CPtrDeclr _ _) -> p (TypePtr ct)
      (CArrDeclr _                         (CNoArrSize _) _) -> p (TypeArray ct 0)
      (CArrDeclr _                         (CArrSize _ _) _) -> p (TypeArray ct 0) -- FIXME
      (CFunDeclr (Right (params, varargs)) _              _) -> p
        (TypeFunction
          ct
          (filter
            (\(_, t) -> t /= (TypeBasicType BasicTypeVoid))
            [ let (_, typeSpec', init) = decomposeCDecl p
              in  let (name, derivedSpec') =
                        if null init then ("_", []) else (head init)
                  in  (name, parseType m typeSpec' derivedSpec')
            | p <- params
            ]
          )
          varargs
          []
        )
      _ -> p ct
parseDerivedType m [] ct = ct

-- Parse CTypeSpecs to find the type of a function/variable declaration
parseDeclSpec :: ModulePath -> [CTypeSpec] -> ConcreteType
parseDeclSpec modPath x = _parseDeclSpec modPath x 0 True False
_parseDeclSpec modPath (h : t) width signed float = case h of
  -- simple types; narrow the definition with each specifier
  (CVoidType   _           ) -> TypeBasicType BasicTypeVoid
  (CBoolType   _           ) -> TypeBasicType BasicTypeBool
  (CSignedType _           ) -> _parseDeclSpec modPath t width True float
  (CUnsigType  _           ) -> _parseDeclSpec modPath t width False float
  (CFloatType  _           ) -> _parseDeclSpec modPath t 32 signed True
  (CDoubleType _           ) -> _parseDeclSpec modPath t 64 signed True
  (CCharType   _           ) -> TypeBasicType $ BasicTypeCChar
  (CIntType    _           ) -> TypeBasicType $ BasicTypeCInt
  (CShortType  _           ) -> _parseDeclSpec modPath t 16 signed False
  (CLongType _) -> _parseDeclSpec modPath t (width + 32) signed False
  (CTypeDef (Ident x _ _) _) -> case x of
    "size_t"   -> TypeBasicType $ BasicTypeCSize
    "int8_t"   -> TypeBasicType $ BasicTypeInt 8
    "int16_t"  -> TypeBasicType $ BasicTypeInt 16
    "int32_t"  -> TypeBasicType $ BasicTypeInt 32
    "int64_t"  -> TypeBasicType $ BasicTypeInt 64
    "uint8_t"  -> TypeBasicType $ BasicTypeUint 8
    "uint16_t" -> TypeBasicType $ BasicTypeUint 16
    "uint32_t" -> TypeBasicType $ BasicTypeUint 32
    "uint64_t" -> TypeBasicType $ BasicTypeUint 64
    "FILE"     -> TypeBasicType $ BasicTypeCFile
    _          -> TypeTypedef (s_pack x)
  -- anonymous structs/enums; TODO: need to generate a stub declaration for these
  (CSUType (CStruct tag (Just (Ident x _ _)) _ _ _) _) ->
    (TypeInstance (modPath, (s_pack x)) [])
  (CSUType (CStruct tag Nothing fields _ _) _) ->
    let fields' = case fields of
          Just f  -> f
          Nothing -> []
    in  (if tag == CStructTag then TypeAnonStruct else TypeAnonUnion)
          [ (name, parseType modPath typeSpec declr)
          | f <- fields'
          , let (_, typeSpec, init) = decomposeCDecl f
          , (name, declr) <- init
          ]
  (CEnumType (CEnum (Just (Ident x _ _)) _ _ _) _) ->
    TypeInstance (modPath, (s_pack x)) []
  (CEnumType (CEnum Nothing (Just variants) _ _) _) -> TypeAnonEnum
    ([ s_pack $ case fst variant of
         Ident x _ _ -> x
     | variant <- variants
     ]
    )
  _ -> _parseDeclSpec modPath t width signed float
_parseDeclSpec modPath [] 0     _      _     = (TypeBasicType BasicTypeUnknown)
_parseDeclSpec modPath [] width signed float = if float
  then TypeBasicType $ BasicTypeFloat width
  else if signed
    then TypeBasicType $ BasicTypeInt width
    else TypeBasicType $ BasicTypeUint width

addCDecl :: CompileContext -> Module -> Str -> ConcreteType -> Span -> IO ()
addCDecl ctx mod name t pos = do
  let bindingData = case t of
        TypeFunction t argTypes isVariadic _ -> FunctionBinding
          (newFunctionDefinition
            { functionName    = ([], name)
            , functionMeta    = [metaExtern]
            , functionPos     = pos
            , functionType    = t
            , functionArgs    = [ newArgSpec { argName    = argName
                                             , argType    = argType
                                             , argDefault = Nothing
                                             }
                                | (argName, argType) <- argTypes
                                ]
            , functionVarargs = isVariadic
            }
          )
        _ -> VarBinding
          (newVarDefinition { varName    = ([], name)
                            , varMeta    = [metaExtern]
                            , varPos     = pos
                            , varType    = t
                            , varDefault = Nothing
                            }
          )
  addBinding ctx ([], name) bindingData
  return ()

isTypedef :: [CStorageSpec] -> Bool
isTypedef ((CTypedef _) : _) = True
isTypedef (h            : t) = isTypedef t
isTypedef []                 = False

defineNamedStructsEnumsUnions
  :: CompileContext -> Module -> Span -> [CTypeSpec] -> IO ()
defineNamedStructsEnumsUnions ctx mod pos [] = do
  return ()
defineNamedStructsEnumsUnions ctx mod pos (h : t) = do
  case h of
    (CSUType (CStruct tag (Just (Ident name _ _)) fields _ _) _) -> do
      let fields' = case fields of
            Just f  -> f
            Nothing -> []
      let fields =
            [ (newVarDefinition) { varName = (modPath mod, fieldName)
                                 , varType = fieldType
                                 }
            | field                  <- fields'
            , (fieldName, fieldType) <- decomposeStructField (modPath mod) field
            ]
      let typeDef =
            ((newTypeDefinition)
              { typeName    = ([], s_pack name)
              , typeMeta    = [metaExtern]
              , typeSubtype = if tag == CStructTag
                then Struct {structFields = fields}
                else Union {unionFields = fields}
              }
            )
      addBinding ctx ([], s_pack name) (TypeBinding typeDef)
      veryNoisyDebugLog ctx $ "define struct " ++ name
    (CEnumType (CEnum (Just (Ident name _ _)) variants _ _) _) -> do
      let variants' = case variants of
            Just v  -> v
            Nothing -> []
      let
        typeDef
          = (((newTypeDefinition) :: TypeDefinition TypedExpr ConcreteType)
              { typeName    = ([], s_pack name)
              , typeMeta    = [metaExtern]
              , typeSubtype = Enum
                { enumUnderlyingType = TypeBasicType BasicTypeVoid
                , enumVariants       = [ newEnumVariant
                                           { variantName = ([], s_pack variantName)
                                           , variantMeta = [metaExtern]
                                           }
                                       | (Ident variantName _ _, _) <- variants'
                                       ]
                }
              }
            )
      let ct = (TypeInstance ([], s_pack name) [])
      addBinding ctx ([], s_pack name) (TypeBinding typeDef)
      veryNoisyDebugLog ctx $ "define enum " ++ name
      forM_
        (enumVariants $ typeSubtype typeDef)
        (\variant -> do
          addBinding ctx
                     ([], tpName $ variantName variant)
                     (EnumConstructor variant)
          veryNoisyDebugLog ctx
            $  "define enum constructor "
            ++ (s_unpack $ tpName $ variantName variant)
        )
    _ -> return ()
  defineNamedStructsEnumsUnions ctx mod pos t

decomposeStructField modPath cdecl =
  let (_, typeSpec, init) = decomposeCDecl cdecl
  in  [ (name, parseType modPath typeSpec declr) | (name, declr) <- init ]

cDeclrName :: CDeclr -> Str
cDeclrName (CDeclr (Just (Ident x _ _)) _ _ _ _) = s_pack x
cDeclrName (CDeclr Nothing              _ _ _ _) = "_"

cDeclrDerived :: CDeclr -> [CDerivedDeclr]
cDeclrDerived (CDeclr _ derived _ _ _) = derived

type DecomposedCDecl = ([CStorageSpec], [CTypeSpec], [(Str, [CDerivedDeclr])])

decomposeCDecl :: CDecl -> DecomposedCDecl
decomposeCDecl (CDecl spec [] _) =
  let (storageSpec, _, _, typeSpec, _, _) = partitionDeclSpecs spec
  in  (storageSpec, typeSpec, [])
decomposeCDecl (CDecl spec xs _) =
  let (storageSpec, _, _, typeSpec, _, _) = partitionDeclSpecs spec
  in  ( storageSpec
      , typeSpec
      , [ ( case declr of
            Just d  -> cDeclrName d
            Nothing -> "_"
          , case declr of
            Just d  -> cDeclrDerived d
            Nothing -> []
          )
        | (declr, _, _) <- xs
        ]
      )
