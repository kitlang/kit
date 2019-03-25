module Kit.Compiler.Passes.IncludeCModules
  ( includeCModules
  , parseCHeader
  )
where

import Control.Monad
import Data.Maybe
import Data.Mutable
import Data.List
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Language.C.Clang
import Language.C.Clang.Cursor
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser
import Kit.Str
import Kit.Toolchain

{-
  For each C header discovered during the BuildModuleGraph pass, parse the
  header to discover all declarations, and make these available from Kit.
-}
includeCModules :: CompileContext -> Toolchain -> IO ()
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
  handle        <- openFile clibPath WriteMode
  systemDefines <- getSystemDefines ctx cc
  hPutStrLn handle systemDefines
  forM_ (nub includes)
    $ \include -> hPutStrLn handle $ "#include \"" ++ include ++ "\""
  hClose handle
  -- include our combined header
  includeCHeader ctx cc mod clibPath
  return ()

includeCHeader :: CompileContext -> Toolchain -> Module -> FilePath -> IO ()
includeCHeader ctx cc mod path = do
  -- parseCMacros ctx cc mod path
  parseCHeader ctx cc mod path

headerParseFlags ctx cc = do
  defaultSearchPaths <- findIncludePaths (ccPath cc)
  let defaultFlags =
        (filter (\flag -> flag /= "-pedantic") $ getCppFlags cc True) ++ ["-w"]
  return
    $  ("-nostdinc")
    :  ("-undef")
    :  (foldr (++) [] [ ["-isystem", path] | path <- defaultSearchPaths ])
    ++ defaultFlags

getSystemDefines ctx cc = readProcess (ccPath cc) ["-dM", "-E", "-"] ""

parseCHeader :: CompileContext -> Toolchain -> Module -> FilePath -> IO ()
parseCHeader ctx cc mod path = do
  flags       <- headerParseFlags ctx cc
  clangIndex  <- createIndex
  parseResult <- parseTranslationUnit clangIndex (ccPath cc) path flags
  let root  = translationUnitCursor parseResult
      decls = cursorChildren root
  forM_ decls $ parseCDecl ctx mod path Nothing

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

annotatedPos x = case cursorExtent x of
  Just range ->
    let (start, end) =
          ( spellingLocation $ rangeStart range
          , spellingLocation $ rangeEnd range
          )
    in  sp (s_unpack $ fileName $ Language.C.Clang.file start)
           (fromIntegral $ line start)
           (fromIntegral $ column start)
           (fromIntegral $ line end)
           (fromIntegral $ column end)
  Nothing -> NoPos

parseCDecl
  :: CompileContext
  -> Module
  -> FilePath
  -> Maybe Str
  -> Cursor
  -> IO (Maybe ConcreteType)
parseCDecl ctx mod path typedefName decl = case cursorKind decl of
  MacroDefinition -> do
    let macroName = cursorSpelling decl
    let children  = cursorChildren decl
    -- print macroName
    -- print $ cursorExtent decl
    -- let body = tokenize $ fromJust $ cursorExtent decl
    -- print (macroName, map tokenSpelling $ tokenSetTokens body)
    tv <- makeTypeVar ctx NoPos
    veryNoisyDebugLog ctx
      $  "Creating macro binding for identifier: "
      ++ s_unpack macroName
    h_insert
      (ctxBindings ctx)
      ([], macroName)
      (ExprBinding $ makeExprTyped (Identifier (Var ([], macroName))) tv NoPos)
    return Nothing

  EnumDecl -> do
    let name = fromMaybe (cursorSpelling decl) typedefName
    decl <- case cursorDefinition decl of
      Just x  -> return x
      Nothing -> return decl
    let pos = annotatedPos decl
    let variants =
          [ cursorSpelling child
          | child <- cursorChildren decl
          , cursorKind child == EnumConstantDecl
          ]
    case name of
      ""   -> return Nothing
      name -> do
        let typeDecl =
              (((newTypeDefinition) :: TypeDefinition TypedExpr ConcreteType)
                { typeName    = ([], name)
                , typeMeta    = [meta metaExtern]
                , typeSubtype = Kit.Ast.Enum
                  { enumUnderlyingType = TypeInt 0
                  , enumVariants       = [ newEnumVariant
                                             { variantName   = ([], variantName)
                                             , variantParent = ([], name)
                                             , variantMeta   = [meta metaExtern]
                                             }
                                         | variantName <- variants
                                         ]
                  }
                }
              )
        when (isNothing typedefName)
          $ addBinding ctx ([], name) (TypeBinding typeDecl)
        veryNoisyDebugLog ctx $ "define enum " ++ s_unpack name

        case typedefName of
          Just _ -> do
            let ct = TypeAnonEnum typedefName variants
            forM_
              variants
              (\variant -> do
                addBinding
                  ctx
                  ([], variant)
                  ( ExprBinding
                  $ makeExprTyped (Identifier $ Var ([], variant)) ct pos
                  )
              )
            return $ Just ct
          Nothing -> do
            forM_
              (enumVariants $ typeSubtype typeDecl)
              (\variant -> do
                addBinding ctx
                           ([], tpName $ variantName variant)
                           (EnumConstructor variant)
                veryNoisyDebugLog ctx
                  $  "define enum constructor "
                  ++ (s_unpack $ tpName $ variantName variant)
              )
            return $ Just $ TypeInstance ([], name) []

  structOrUnion | (structOrUnion == StructDecl || structOrUnion == UnionDecl) ->
    do
      let name = fromMaybe (cursorSpelling decl) typedefName
      decl <- case cursorDefinition decl of
        Just x  -> return x
        Nothing -> return decl
      case name of
        ""   -> return Nothing
        name -> do
          let pos = annotatedPos decl
          let fields =
                [ (cursorSpelling child, parseType $ cursorType child)
                | child <- cursorChildren decl
                , cursorKind child == FieldDecl
                ]
          let
            ct = case typedefName of
              Just _ ->
                (if structOrUnion == StructDecl
                    then TypeAnonStruct
                    else TypeAnonUnion
                  )
                  typedefName
                  fields
              _ -> TypeInstance ([], name) []
          let fieldDefs =
                [ (newVarDefinition) { varName = (modPath mod, fieldName)
                                     , varType = fieldType
                                     }
                | (fieldName, fieldType) <- fields
                ]
          let typeDecl =
                ((newTypeDefinition)
                  { typeName    = ([], name)
                  , typeMeta    = [meta metaExtern]
                  , typeSubtype = StructUnion
                    { structUnionFields = fieldDefs
                    , isStruct          = structOrUnion == StructDecl
                    }
                  }
                )
          when (isNothing typedefName)
            $ addBinding ctx ([], name) (TypeBinding typeDecl)
          return $ Just ct

  TypedefDecl -> do
    let name = cursorSpelling decl
    let pos  = annotatedPos decl
    let children =
          [ child
          | child <- cursorChildren decl
          , isJust
            $ elemIndex (cursorKind child) [StructDecl, UnionDecl, EnumDecl]
          ]
    case children of
      [] -> do
        h_insert (ctxTypedefs ctx) name
          $ parseType
          $ cursorTypedefDeclUnderlyingType decl
        return ()
      (child : _) -> do
        result <- parseCDecl
          ctx
          mod
          path
          (if cursorSpelling child == "" then Just name else Nothing)
          child
        h_insert (ctxTypedefs ctx) name $ fromJust result
    return Nothing

  VarDecl -> do
    let name = cursorSpelling decl
    let pos  = annotatedPos decl
    let t    = parseType $ cursorType decl
    noisyDebugLog ctx $ "bind variable " ++ (s_unpack name) ++ ": " ++ (show t)
    addCDecl ctx mod name t pos
    return Nothing

  FunctionDecl -> do
    let name = cursorSpelling decl
    let pos  = annotatedPos decl
    let t    = parseType $ cursorType decl
    noisyDebugLog ctx $ "bind function " ++ (s_unpack name) ++ ": " ++ (show t)
    addCDecl ctx mod name t pos
    return Nothing

  _ -> do
    return Nothing

parseType :: Maybe Type -> ConcreteType
parseType Nothing = TypeBasicType BasicTypeUnknown
parseType (Just t) =
  let
    t' = typeCanonicalType t
    f proto = case typeArgs t of
      Just x -> TypeFunction
        (parseType $ typeResultType t)
        [ newArgSpec { argName = "_", argType = t }
        | t <- map (parseType . Just) x
        , t /= TypeBasicType BasicTypeVoid
        ]
        (if typeIsVariadic t' && proto then Just "" else Nothing)
        []
      Nothing -> TypeBasicType BasicTypeUnknown
    td = case typeDeclaration t of
      Just decl -> case cursorSpelling decl of
        "size_t"   -> TypeSize
        "int8_t"   -> TypeInt 8
        "int16_t"  -> TypeInt 16
        "int32_t"  -> TypeInt 32
        "int64_t"  -> TypeInt 64
        "uint8_t"  -> TypeUint 8
        "uint16_t" -> TypeUint 16
        "uint32_t" -> TypeUint 32
        "uint64_t" -> TypeUint 64
        x          -> case typeDeclaration t of
          Just x | cursorType x /= Just t -> parseType $ cursorType x
          _                               -> TypeTypedef x
      _ -> TypeBasicType BasicTypeUnknown
    result = case typeKind t of
      Language.C.Clang.Void       -> TypeBasicType BasicTypeVoid
      Language.C.Clang.UInt       -> TypeUint 0
      Language.C.Clang.UChar      -> TypeUint 8
      Language.C.Clang.UShort     -> TypeUint 16
      Language.C.Clang.ULong      -> TypeUint 32
      Language.C.Clang.ULongLong  -> TypeUint 64
      Language.C.Clang.SChar      -> TypeChar
      Language.C.Clang.Char_S     -> TypeChar
      Language.C.Clang.Int        -> TypeInt 0
      Language.C.Clang.Short      -> TypeInt 16
      Language.C.Clang.Long       -> TypeInt 32
      Language.C.Clang.LongLong   -> TypeInt 64
      Language.C.Clang.Float      -> TypeFloat 32
      Language.C.Clang.Double     -> TypeFloat 64
      Language.C.Clang.LongDouble -> TypeFloat 64
      Language.C.Clang.Enum       -> case typeDeclaration t of
        Just decl -> if cursorSpelling decl == ""
          then TypeAnonEnum
            Nothing
            [ cursorSpelling child
            | child <- cursorChildren decl
            , cursorKind child == EnumConstantDecl
            ]
          else TypeInstance ([], cursorSpelling decl) []
        Nothing -> TypeInstance ([], typeSpelling t) []
      Language.C.Clang.Typedef -> td
      Language.C.Clang.Record  -> case typeDeclaration t of
        Just decl -> if cursorSpelling decl == ""
          then
            (if cursorKind decl == StructDecl
                then TypeAnonStruct
                else TypeAnonUnion
              )
              Nothing
              [ (cursorSpelling child, parseType $ cursorType child)
              | child <- cursorChildren decl
              , cursorKind child == FieldDecl
              ]
          else TypeInstance ([], cursorSpelling decl) []
        Nothing -> TypeInstance ([], typeSpelling t) []
      Language.C.Clang.Pointer -> TypePtr $ parseType $ typePointeeType t
      Language.C.Clang.FunctionProto   -> f True
      Language.C.Clang.FunctionNoProto -> f False
      Language.C.Clang.ConstantArray   -> TypeArray
        (parseType $ typeElementType t)
        (case typeArraySize t of
          Just x  -> fromIntegral x
          Nothing -> 0
        )
      Language.C.Clang.IncompleteArray ->
        TypeArray (parseType $ typeElementType t) 0
      Language.C.Clang.Unexposed -> case typeKind t' of
        Language.C.Clang.Unexposed -> f True
        x                          -> parseType $ Just t'
      x -> TypeBasicType BasicTypeUnknown
  in
    if typeIsConstQualified t then TypeConst result else result

addCDecl :: CompileContext -> Module -> Str -> ConcreteType -> Span -> IO ()
addCDecl ctx mod name t pos = do
  let bindingData = case t of
        TypeFunction t argSpecs isVariadic _ -> FunctionBinding
          (newFunctionDefinition { functionName   = ([], name)
                                 , functionMeta   = [meta metaExtern]
                                 , functionPos    = pos
                                 , functionType   = t
                                 , functionArgs   = argSpecs
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
