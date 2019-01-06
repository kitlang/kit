module Kit.Compiler.Passes.IncludeCModules
  ( includeCModules
  , parseCHeader
  , parseType
  )
where

import           Control.Monad
import           Data.Mutable
import           Data.List
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process
import           Language.C
import           Language.C.Analysis.ConstEval
import           Language.C.Data.Ident
import           Language.C.Data.Position
import           Language.C.System.GCC
import           Kit.Ast
import           Kit.Compiler.Binding
import           Kit.Compiler.CCompiler
import           Kit.Compiler.Context
import           Kit.Compiler.Module
import           Kit.Compiler.TypedExpr
import           Kit.Compiler.Utils
import           Kit.Error
import           Kit.HashTable
import           Kit.Log
import           Kit.Parser
import           Kit.Str

{-
  For each C header discovered during the BuildModuleGraph pass, parse the
  header to discover all declarations, and make these available from Kit.
-}
includeCModules :: CompileContext -> CCompiler -> IO ()
includeCModules ctx cc = do
  includes <- readRef (ctxIncludes ctx)
  existing <- h_lookup (ctxModules ctx) externModPath
  mod      <- case existing of
    Just x  -> return x
    Nothing -> do
      mod <- newCMod
      h_insert (ctxModules ctx) externModPath mod
      return mod
  -- to speed things up, create just one header that includes all others
  let clibPath = (takeDirectory $ includePath ctx) </> "__clibs.h"
  createDirectoryIfMissing True $ takeDirectory clibPath
  handle <- openFile clibPath WriteMode
  forM_ (nub includes)
    $ \include -> hPutStrLn handle $ "#include \"" ++ include ++ "\""
  hClose handle
  -- include our combined header
  includeCHeader ctx cc mod clibPath
  return ()

includeCHeader :: CompileContext -> CCompiler -> Module -> FilePath -> IO ()
includeCHeader ctx cc mod path = do
  parseCMacros ctx cc mod path
  parseCHeader ctx cc mod path

headerParseFlags ctx cc = do
  flags <- getCompileFlags ctx cc
  return $ (filter (\flag -> flag /= "-pedantic") $ flags) ++ ["-w"]

parseCMacros ctx cc mod path = do
  flags  <- headerParseFlags ctx cc
  result <- readProcess (ccPath cc) ("-dM" : "-E" : flags ++ [path]) ""
  forM_ (lines result) $ \line -> do
    when (isPrefixOf "#define " line) $ do
      let definition             = drop (length ("#define " :: String)) line
      let (macroName : macroDef) = words definition
      -- for now we're ignoring any macros that take arguments
      unless ((null macroDef) || (elem '(' macroName)) $ do
        let input = inputStreamFromString $ intercalate " " macroDef
        let result =
              execParser expressionP input nopos builtinTypeNames newNameSupply
        case result of
          Right ((CVar (Ident name _ _) _), _) -> do
            -- identifiers
            tv <- makeTypeVar ctx NoPos
            veryNoisyDebugLog ctx
              $  "Creating macro binding for identifier: "
              ++ macroName
            h_insert
              (ctxBindings ctx)
              ([], s_pack macroName)
              ( ExprBinding
              $ makeExprTyped (Identifier (Var ([], s_pack name))) tv NoPos
              )
          Right (x, _) -> do
            -- for now we'll be lazy and only support Int constants...
            case intValue x of
              Just x -> do
                veryNoisyDebugLog ctx
                  $  "Creating macro binding for int constant: "
                  ++ macroName
                h_insert
                  (ctxBindings ctx)
                  ([], s_pack macroName)
                  (ExprBinding $ makeExprTyped
                    (Identifier (MacroVar (s_pack macroName) $ TypeInt 0))
                    (TypeInt 0)
                    NoPos
                  )
              Nothing -> return ()
          _ -> return ()

parseCHeader :: CompileContext -> CCompiler -> Module -> FilePath -> IO ()
parseCHeader ctx cc mod path = do
  flags       <- headerParseFlags ctx cc
  parseResult <- parseCFile (newGCC $ ccPath cc) Nothing flags path
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
      let (declSpec, initializers) = decomposeCDecl cdecl
      if isTypedef declSpec
        then do
          -- this is a typedef
          forM_ initializers (defineTypedef ctx mod declSpec pos)
          -- if there's a named struct/enum/union, even in a typedef, define it here
          defineNamedStructsEnumsUnions ctx mod pos declSpec
        else if null initializers
          then -- this is a non-typedef struct, enum or union declaration
               defineNamedStructsEnumsUnions ctx mod pos declSpec
          else -- this is one or more variable/function declarations
               forM_
            (initializers)
            (\(name, declr) -> do
              let t' = parseType (modPath mod) declSpec declr
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
  -> [CDeclSpec]
  -> Span
  -> (Str, [CDerivedDeclr])
  -> IO ()
defineTypedef ctx mod declSpec pos (name, declr) = do
  let t' = parseType (modPath mod) declSpec declr
  t' <- return $ case t' of
    TypeAnonStruct Nothing x -> TypeAnonStruct (Just name) x
    TypeAnonUnion  Nothing x -> TypeAnonUnion (Just name) x
    TypeAnonEnum   Nothing x -> TypeAnonEnum (Just name) x
    _                        -> t'
  veryNoisyDebugLog ctx $ "typedef " ++ (s_unpack name) ++ ": " ++ (show t')
  when (t' == TypeBasicType BasicTypeUnknown)
    $ unknownTypeWarning ctx mod name pos
  h_insert (ctxTypedefs ctx) name t'
  case t' of
    TypeAnonEnum _ variants -> do
      forM_
        variants
        (\variant -> do
          addBinding
            ctx
            ([], variant)
            (VarBinding $ newVarDefinition { varName    = ([], variant)
                                           , varMeta    = [meta metaExtern]
                                           , varPos     = pos
                                           , varType    = t'
                                           , varDefault = Nothing
                                           }
            )
          veryNoisyDebugLog ctx
            $  "define enum constructor "
            ++ (s_unpack $ variant)
        )
    _ -> return ()

parseType :: ModulePath -> [CDeclSpec] -> [CDerivedDeclr] -> ConcreteType
parseType m typeSpec declr =
  parseDerivedType m (reverse declr) (parseDeclSpec m typeSpec)
parseDerivedType m (h' : t') ct =
  let p = parseDerivedType m t'
  in  case h' of
        (CPtrDeclr _ _) -> p $ if ct == (TypeBasicType BasicTypeCChar)
          then TypeInstance (["kit", "common"], "CString") []
          else (TypePtr ct)
        (CArrDeclr _ (CNoArrSize _) _) -> p (TypeArray ct 0)
        (CArrDeclr _ (CArrSize _ _) _) -> p (TypeArray ct 0) -- FIXME
        (CFunDeclr (Right (params, varargs)) _ _) -> p
          (TypeFunction
            ct
            (ConcreteArgs $ filter
              (\(_, t) -> t /= TypeBasicType BasicTypeVoid)
              [ let (declSpec', init) = decomposeCDecl p
                in  let (name, derivedSpec') =
                          if null init then ("_", []) else (head init)
                    in  (name, parseType m declSpec' derivedSpec')
              | p <- params
              ]
            )
            (if varargs then Just "" else Nothing)
            []
          )
        _ -> p ct
parseDerivedType m [] ct = ct

-- Parse CTypeSpecs to find the type of a function/variable declaration
parseDeclSpec :: ModulePath -> [CDeclSpec] -> ConcreteType
parseDeclSpec modPath x = _parseDeclSpec modPath x 0 True False
_parseDeclSpec modPath (h : t) width signed float = case h of
  -- simple types; narrow the definition with each specifier
  (CTypeSpec (CVoidType   _)) -> TypeBasicType BasicTypeVoid
  (CTypeSpec (CBoolType   _)) -> TypeBool
  (CTypeSpec (CSignedType _)) -> _parseDeclSpec modPath t width True float
  (CTypeSpec (CUnsigType  _)) -> _parseDeclSpec modPath t width False float
  (CTypeSpec (CFloatType  _)) -> _parseDeclSpec modPath t 32 signed True
  (CTypeSpec (CDoubleType _)) -> _parseDeclSpec modPath t 64 signed True
  (CTypeSpec (CCharType   _)) -> TypeChar
  (CTypeSpec (CIntType    _)) -> if signed then TypeInt 0 else TypeUint 0
  (CTypeSpec (CShortType  _)) -> _parseDeclSpec modPath t 16 signed False
  (CTypeSpec (CLongType _)) ->
    _parseDeclSpec modPath t (width + 32) signed False
  (CTypeSpec (CTypeDef (Ident x _ _) _)) -> case x of
    "size_t"   -> TypeSize
    "int8_t"   -> TypeInt 8
    "int16_t"  -> TypeInt 16
    "int32_t"  -> TypeInt 32
    "int64_t"  -> TypeInt 64
    "uint8_t"  -> TypeUint 8
    "uint16_t" -> TypeUint 16
    "uint32_t" -> TypeUint 32
    "uint64_t" -> TypeUint 64
    _          -> TypeTypedef (s_pack x)
  -- anonymous structs/enums; TODO: need to generate a stub declaration for these
  (CTypeSpec (CSUType (CStruct tag (Just (Ident x _ _)) _ _ _) _)) ->
    (TypeInstance (modPath, (s_pack x)) [])
  (CTypeSpec (CSUType (CStruct tag Nothing fields _ _) _)) ->
    let fields' = case fields of
          Just f  -> f
          Nothing -> []
    in  (if tag == CStructTag then TypeAnonStruct else TypeAnonUnion)
          Nothing
          [ (name, parseType modPath declSpec declr)
          | f <- fields'
          , let (declSpec, init) = decomposeCDecl f
          , (name, declr) <- init
          ]
  (CTypeSpec (CEnumType (CEnum (Just (Ident x _ _)) _ _ _) _)) ->
    TypeInstance (modPath, (s_pack x)) []
  (CTypeSpec (CEnumType (CEnum Nothing (Just variants) _ _) _)) -> TypeAnonEnum
    Nothing
    ([ s_pack $ case fst variant of
         Ident x _ _ -> x
     | variant <- variants
     ]
    )
  (CTypeQual (CConstQual _)) ->
    TypeConst $ _parseDeclSpec modPath t width signed float
  _ -> _parseDeclSpec modPath t width signed float
_parseDeclSpec modPath [] 0     False  _     = (TypeUint 0)
_parseDeclSpec modPath [] 0     _      _     = (TypeBasicType BasicTypeUnknown)
_parseDeclSpec modPath [] width signed float = if float
  then TypeFloat width
  else if signed then TypeInt width else TypeUint width

addCDecl :: CompileContext -> Module -> Str -> ConcreteType -> Span -> IO ()
addCDecl ctx mod name t pos = do
  let bindingData = case t of
        TypeFunction t (ConcreteArgs argTypes) isVariadic _ -> FunctionBinding
          (newFunctionDefinition
            { functionName   = ([], name)
            , functionMeta   = [meta metaExtern]
            , functionPos    = pos
            , functionType   = t
            , functionArgs   = [ newArgSpec { argName    = argName
                                            , argType    = argType
                                            , argDefault = Nothing
                                            }
                               | (argName, argType) <- argTypes
                               ]
            , functionVararg = isVariadic
            }
          )
        _ -> VarBinding
          (newVarDefinition { varName    = ([], name)
                            , varMeta    = [meta metaExtern]
                            , varPos     = pos
                            , varType    = t
                            , varDefault = Nothing
                            }
          )
  addBinding ctx ([], name) bindingData
  return ()

isTypedef :: [CDeclSpec] -> Bool
isTypedef ((CStorageSpec (CTypedef _)) : _) = True
isTypedef (h : t) = isTypedef t
isTypedef [] = False

defineNamedStructsEnumsUnions
  :: CompileContext -> Module -> Span -> [CDeclSpec] -> IO ()
defineNamedStructsEnumsUnions ctx mod pos [] = do
  return ()
defineNamedStructsEnumsUnions ctx mod pos (h : t) = do
  case h of
    (CTypeSpec (CSUType (CStruct tag (Just (Ident name _ _)) fields _ _) _)) ->
      do
        let fields' = case fields of
              Just f  -> f
              Nothing -> []
        let
          fields =
            [ (newVarDefinition) { varName = (modPath mod, fieldName)
                                 , varType = fieldType
                                 }
            | field                  <- fields'
            , (fieldName, fieldType) <- decomposeStructField (modPath mod) field
            ]
        let typeDef =
              ((newTypeDefinition)
                { typeName    = ([], s_pack name)
                , typeMeta    = [meta metaExtern]
                , typeSubtype = StructUnion { structUnionFields = fields
                                            , isStruct = tag == CStructTag
                                            }
                }
              )
        addBinding ctx ([], s_pack name) (TypeBinding typeDef)
    (CTypeSpec (CEnumType (CEnum (Just (Ident name _ _)) variants _ _) _)) ->
      do
        let variants' = case variants of
              Just v  -> v
              Nothing -> []
        let
          typeDef =
            (((newTypeDefinition) :: TypeDefinition TypedExpr ConcreteType)
              { typeName    = ([], s_pack name)
              , typeMeta    = [meta metaExtern]
              , typeSubtype =
                Enum
                  { enumUnderlyingType = TypeBasicType BasicTypeVoid
                  , enumVariants = [ newEnumVariant
                                       { variantName = ([], s_pack variantName)
                                       , variantParent = ([], s_pack name)
                                       , variantMeta = [meta metaExtern]
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
  let (declSpec, init) = decomposeCDecl cdecl
  in  [ (name, parseType modPath declSpec declr) | (name, declr) <- init ]

cDeclrName :: CDeclr -> Str
cDeclrName (CDeclr (Just (Ident x _ _)) _ _ _ _) = s_pack x
cDeclrName (CDeclr Nothing              _ _ _ _) = "_"

cDeclrDerived :: CDeclr -> [CDerivedDeclr]
cDeclrDerived (CDeclr _ derived _ _ _) = derived

type CTypeSpecOrQual = Either CTypeSpec CTypeQual
type DecomposedCDecl = ([CDeclSpec], [(Str, [CDerivedDeclr])])

decomposeCDecl :: CDecl -> DecomposedCDecl
decomposeCDecl (CDecl spec xs _) =
  ( spec
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
