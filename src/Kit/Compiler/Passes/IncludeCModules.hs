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

includeCModules :: CompileContext -> IO ()
includeCModules ctx = do
  includes <- readIORef (ctxIncludes ctx)
  forM_ includes (includeCHeader ctx)
  return ()

includeCHeader :: CompileContext -> FilePath -> IO Module
includeCHeader ctx path = do
  let modPath = includeToModulePath path
  existing <- h_lookup (ctxModules ctx) modPath
  case existing of
    Just x -> do
      return x
    Nothing -> do
      found <- findSourceFile path (ctxIncludePaths ctx)
      case found of
        Just f -> do
          debugLog ctx $ "found header " ++ show path ++ " at " ++ show f
          mod <- newCMod path
          parseCHeader ctx              mod     f
          h_insert     (ctxModules ctx) modPath mod
          names <- bindingNames (modScope mod)
          forM_ names (addGlobalName ctx mod null_span)
          return mod
        Nothing -> throwk
          $ IncludeError path [ (dir </> path) | dir <- ctxIncludePaths ctx ]

parseCHeader :: CompileContext -> Module -> FilePath -> IO ()
parseCHeader ctx mod path = do
  parseResult <- parseCFile (newGCC "gcc")
                            Nothing
                            [ "-I" ++ dir | dir <- ctxIncludePaths ctx ]
                            path
  case parseResult of
    Left e -> throwk $ BasicError
      ("Parsing C header " ++ show path ++ " failed: " ++ show e)
      Nothing
    Right (CTranslUnit decls _) -> do
      parseCDecls ctx mod decls

unknownTypeWarning :: CompileContext -> Module -> Str -> IO ()
unknownTypeWarning ctx mod name = do
  warningLog
    $  "couldn't determine type of "
    ++ (s_unpack name)
    ++ " in "
    ++ (show mod)
    ++ "; attempts to access values of this type will fail"

parseCDecls :: CompileContext -> Module -> [CExtDecl] -> IO ()
parseCDecls ctx mod [] = do
  return ()
parseCDecls ctx mod (h : t) = do
  case h of
    CDeclExt cdecl -> do
      let (storageSpec, typeSpec, initializers) = decomposeCDecl cdecl
      if isTypedef storageSpec
        then do
          -- this is a typedef
          forM_ initializers (defineTypedef ctx mod typeSpec)
          -- if there's a named struct/enum, even in a typedef, define t here
          defineNamedStructsAndEnums ctx mod typeSpec
        else if null initializers
          then -- this is a non-typedef struct, enum or union declaration
               defineNamedStructsAndEnums ctx mod typeSpec
          else -- this is one or more variable/function declarations
               forM_
            (initializers)
            (\(name, declr) -> do
              let t' = parseType (modPath mod) typeSpec (reverse declr)
              case t' of
                TypeBasicType BasicTypeUnknown ->
                  unknownTypeWarning ctx mod name
                _ -> return ()
              debugLog ctx $ "bind " ++ (s_unpack name) ++ ": " ++ (show t')
              addCDecl ctx mod name t'
            )
    _ -> do
      return ()
  parseCDecls ctx mod t

defineTypedef
  :: CompileContext -> Module -> [CTypeSpec] -> (Str, [CDerivedDeclr]) -> IO ()
defineTypedef ctx mod typeSpec (name, declr) = do
  let t' = parseType (modPath mod) typeSpec declr
  debugLog ctx $ "typedef " ++ (s_unpack name) ++ ": " ++ (show t')
  case t' of
    TypeBasicType BasicTypeUnknown -> unknownTypeWarning ctx mod name
    _                              -> return ()
  bindToScope (modScope mod) name (newBinding TypeBinding t' Nothing null_span)

parseType :: ModulePath -> [CTypeSpec] -> [CDerivedDeclr] -> ConcreteType
parseType m typeSpec declr =
  parseDerivedType m declr (parseDeclSpec m typeSpec)
parseDerivedType m (h' : t') ct =
  let p = parseDerivedType m t'
  in
    case h' of
      (CPtrDeclr _ _) -> p (TypePtr ct)
      (CArrDeclr _                         (CNoArrSize _) _) -> p (TypeArr ct Nothing)
      (CArrDeclr _                         (CArrSize _ _) _) -> p (TypeArr ct Nothing) -- FIXME
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
  (CCharType   _           ) -> _parseDeclSpec modPath t 8 signed False
  (CShortType  _           ) -> _parseDeclSpec modPath t 16 signed False
  (CIntType    _           ) -> _parseDeclSpec modPath t 16 signed False
  (CLongType _) -> _parseDeclSpec modPath t (width + 32) signed False
  (CTypeDef (Ident x _ _) _) -> TypeTypedef (modPath, (s_pack x)) []
  -- anonymous structs/enums; TODO: need to generate a stub declaration for these
  (CSUType (CStruct CStructTag (Just (Ident x _ _)) _ _ _) _) ->
    TypeStruct (modPath, (s_pack x)) []
  (CSUType (CStruct CStructTag Nothing fields _ _) _) ->
    let fields' = case fields of
          Just f  -> f
          Nothing -> []
    in  TypeAnonStruct
          [ (name, parseType modPath typeSpec declr)
          | f <- fields'
          , let (_, typeSpec, init) = decomposeCDecl f
          , (name, declr) <- init
          ]
  (CEnumType (CEnum (Just (Ident x _ _)) _ _ _) _) ->
    TypeEnum (modPath, (s_pack x)) []
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

addCDecl :: CompileContext -> Module -> Str -> ConcreteType -> IO ()
addCDecl ctx mod name t = do
  let bindingData = case t of
        TypeFunction t argTypes isVariadic -> FunctionBinding
        _ -> VarBinding
  bindToScope (modScope mod) name (newBinding bindingData t Nothing null_span)
  return ()

isTypedef :: [CStorageSpec] -> Bool
isTypedef ((CTypedef _) : _) = True
isTypedef (h            : t) = isTypedef t
isTypedef []                 = False

defineNamedStructsAndEnums :: CompileContext -> Module -> [CTypeSpec] -> IO ()
defineNamedStructsAndEnums ctx mod [] = do
  return ()
defineNamedStructsAndEnums ctx mod (h : t) = do
  case h of
    (CSUType (CStruct CStructTag (Just (Ident name _ _)) fields _ _) _) -> do
      let fields' = case fields of
            Just f  -> f
            Nothing -> []
      let
        typeDef
          = ((newTypeDefinition (s_pack name))
              { typeNameMangling = Nothing
              , typeType         = Struct
                { structFields = [ (newVarDefinition)
                                      { varName         = fieldName
                                      , varType         = Just
                                        $ ConcreteType
                                        $ fieldType
                                      , varNameMangling = Nothing
                                      }
                                  | field                  <- fields'
                                  , (fieldName, fieldType) <-
                                    decomposeStructField (modPath mod) field
                                  ]
                }
              }
            )
      bindToScope
        (modScope mod)
        (s_pack name)
        (newBinding TypeBinding
                    (TypeStruct (modPath mod, s_pack name) [])
                    Nothing
                    null_span
        )
      bindToScope (modDefinitions mod) (s_pack name) (DefinitionType typeDef)
      debugLog ctx $ "define struct " ++ name
    (CEnumType (CEnum (Just (Ident name _ _)) variants _ _) _) -> do
      let variants' = case variants of
            Just v  -> v
            Nothing -> []
      let
        typeDef =
          (((newTypeDefinition (s_pack name)) :: TypeDefinition
               Expr
               (Maybe TypeSpec)
           )
            { typeNameMangling = Nothing
            , typeType         = Enum
              { enumUnderlyingType = Nothing
              , enumVariants        = [ newEnumVariant
                                           { variantName = s_pack variantName
                                           }
                                       | (Ident variantName _ _, _) <- variants'
                                       ]
              }
            }
          )
      let ct = (TypeEnum (modPath mod, s_pack name) [])
      bindToScope (modScope mod)
                  (s_pack name)
                  (newBinding TypeBinding ct Nothing null_span)
      bindToScope (modDefinitions mod) (s_pack name) (DefinitionType typeDef)
      debugLog ctx $ "define enum " ++ name
    _ -> return ()
  defineNamedStructsAndEnums ctx mod t

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
