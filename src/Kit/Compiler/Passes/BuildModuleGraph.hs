module Kit.Compiler.Passes.BuildModuleGraph where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import System.Directory
import System.FilePath
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

data DuplicateGlobalNameError = DuplicateGlobalNameError ModulePath Str Span Span deriving (Eq, Show)
instance Errable DuplicateGlobalNameError where
  logError e@(DuplicateGlobalNameError mod name pos1 pos2) = do
    logErrorBasic e $ "Duplicate declaration for global name `" ++ s_unpack name ++ "` in " ++ s_unpack (showModulePath mod) ++ "; \n\nFirst declaration:"
    ePutStrLn "\nSecond declaration:"
    displayFileSnippet pos2
    ePutStrLn "\n#[extern] declarations and declarations from included C headers must have globally unique names."
  errPos (DuplicateGlobalNameError _ _ pos _) = Just pos

type SyntacticDecl = Declaration Expr (Maybe TypeSpec)

{-
  Starting from the compilation entry point ("main" module), recursively trace
  all imports and includes to discover the full set of modules and C modules
  that must be built. Circular imports are OK and will be processed only once.

  This step also scans modules for declarations and creates a high level
  interface which can be used in ResolveModuleTypes; we'll know e.g. that type
  X exists and is a struct, but not its fields or types, etc.
-}
buildModuleGraph :: CompileContext -> IO [(Module, [(SyntacticDecl, Span)])]
buildModuleGraph ctx = loadModule ctx (ctxMainModule ctx) Nothing

{-
  Load a module, if it hasn't already been loaded. Also triggers recursive
  loading of the module's imports and initial type resolution of top level
  declarations.
-}
loadModule
  :: CompileContext
  -> ModulePath
  -> Maybe Span
  -> IO [(Module, [(SyntacticDecl, Span)])]
loadModule ctx mod pos = do
  existing <- h_lookup (ctxModules ctx) mod
  case existing of
    Just x  -> return []
    Nothing -> do
      broken <- h_lookup (ctxFailedModules ctx) mod
      case broken of
        Just x -> do
          noisyDebugLog ctx
            $  "skipping known broken module <"
            ++ s_unpack (showModulePath mod)
            ++ ">"
          throwk $ KitErrors []
        Nothing -> do
          (m, decls) <- _loadModule ctx mod pos
          h_insert (ctxModules ctx) mod m
          if modImports m /= []
          then
            noisyDebugLog ctx
            $  "module <"
            ++ s_unpack (showModulePath mod)
            ++ "> imports: "
            ++ (intercalate
                 ", "
                 (map (s_unpack . showModulePath . fst) $ modImports m)
               )
          else
            return ()
          includes <- readIORef (modIncludes m)
          forM_
            includes
            (\(mod', _) ->
              modifyIORef (ctxIncludes ctx) (\current -> mod' : current)
            )
          imports <- forMWithErrors (modImports m) (_loadImportedModule ctx)
          let (errs, results) = foldr
                (\result (errs, results) -> case result of
                  Left  errs'    -> (errs ++ [errs'], results)
                  Right results' -> (errs, results ++ results')
                )
                ([], [])
                imports
          unless ((null :: [KitError] -> Bool) errs)
            $ throwk
            $ KitErrors
            $ nub
            $ reverse errs
          return $ (m, decls) : results


{-
  Find all relevant prelude modules for a package, and return a list of
  expressions to prepend to the module contents.

  Given module pkg1.pkg2.mymod, this searches for:

  - pkg1.pkg2.prelude
  - pkg1.prelude
  - prelude

  and appends the contents of any of these modules that exist in reverse
  order.
-}
_loadPreludes :: CompileContext -> ModulePath -> IO [Statement]
_loadPreludes ctx mod = do
  preludes <- _loadPrelude ctx mod
  if mod == []
    then return preludes
    else do
      _parents <- _loadPreludes ctx (take (length mod - 1) mod)
      return $ _parents ++ preludes

-- Look for a single package prelude. Caches the result.
_loadPrelude :: CompileContext -> ModulePath -> IO [Statement]
_loadPrelude ctx mod = do
  -- look for a possible prelude module for this package
  existing <- h_lookup (ctxPreludes ctx) mod
  case existing of
    Just x  -> return x
    Nothing -> do
      let preludePath = mod ++ ["prelude"]
      noisyDebugLog ctx
        $  "checking for prelude <"
        ++ s_unpack (showModulePath preludePath)
        ++ ">"
      broken <- h_exists (ctxFailedModules ctx) mod
      if broken
        then return []
        else do
          found <-
            try $ findModule ctx preludePath Nothing :: IO
              (Either KitError FilePath)
          case found of
            Left _ -> do
              return []
            Right fp -> do
              (path, preludes) <- parseModuleExprs ctx mod (Just fp) Nothing
              h_insert (ctxPreludes ctx) mod preludes
              return preludes

_loadModule
  :: CompileContext
  -> ModulePath
  -> Maybe Span
  -> IO (Module, [(SyntacticDecl, Span)])
_loadModule ctx mod pos = do
  (fp, exprs) <- parseModuleExprs ctx mod Nothing pos
  prelude     <- if last mod == "prelude"
    then return []
    else _loadPreludes ctx (take (length mod - 1) mod)
  let stmts = prelude ++ exprs
  m       <- newMod mod fp
  imports <- findImports ctx mod stmts
  let includes   = findIncludes stmts
  let createdMod = m { modImports = imports }
  writeIORef (modIncludes createdMod) includes
  decls <- forMWithErrors stmts (addStmtToModuleInterface ctx m)
  return (createdMod, foldr (++) [] decls)

_loadImportedModule
  :: CompileContext
  -> (ModulePath, Span)
  -> IO (Either KitError [(Module, [(SyntacticDecl, Span)])])
_loadImportedModule ctx (mod, pos) = try $ loadModule ctx mod (Just pos)

parseModuleExprs
  :: CompileContext
  -> ModulePath
  -> Maybe FilePath
  -> Maybe Span
  -> IO (FilePath, [Statement])
parseModuleExprs ctx mod fp pos = do
  path <- case fp of
    Just fp -> do
      return fp
    Nothing -> findModule ctx mod pos
  parsed <- parseFile path
  case parsed of
    ParseResult r -> return (path, r)
    Err         e -> do
      h_insert (ctxFailedModules ctx) mod ()
      throwk e

interfaceTypeConverter
  :: CompileContext
  -> Module
  -> Span
  -> [TypePath]
  -> Maybe TypeSpec
  -> IO ConcreteType
interfaceTypeConverter ctx mod pos typeParams (Just (ConstantTypeSpec v _)) =
  return $ ConstantType v
interfaceTypeConverter ctx mod pos (h : t) x@(Just (TypeSpec tp [] _)) =
  if h == tp || ((null $ fst tp) && tpName tp == tpName h)
    then return $ TypeTypeParam h
    else interfaceTypeConverter ctx mod pos t x
interfaceTypeConverter ctx mod pos typeParams (Just x) =
  return $ UnresolvedType x $ modPath mod
interfaceTypeConverter ctx mod pos typeParams x = makeTypeVar ctx pos

addStmtToModuleInterface
  :: CompileContext -> Module -> Statement -> IO [(SyntacticDecl, Span)]
addStmtToModuleInterface ctx mod s = do
  -- the expressions from these conversions shouldn't be used;
  -- we'll use the actual typed versions generated later
  let interfaceParamConverter params = converter
        (\e -> do
          tv <- makeTypeVar ctx (ePos e)
          return $ makeExprTyped (This) tv (ePos e)
        )
        (\pos t -> interfaceTypeConverter ctx mod pos params t)
  let interfaceConverter = interfaceParamConverter []
  decls <- case stmt s of
    TypeDeclaration d -> do
      let extern = hasMeta "extern" (typeMeta d)
      let name   = tpName $ typeName d
      let tp     = (if extern then [] else modPath mod, name)

      converted <- do
        c <- convertTypeDefinition interfaceParamConverter (d { typeName = tp })
        if null (typeMethods d)
          then return c
          else do
            return $ implicitifyInstanceMethods thisPtrName
                                                (TypePtr TypeSelf)
                                                (\_ -> id)
                                                c

      let b = TypeBinding converted
      when extern $ recordGlobalName $ name
      addToInterface ctx mod name b (not extern) False

      forM_
        (typeStaticFields converted)
        (\field -> do
          addBinding ctx (varName field) (VarBinding field)
        )
      forM_
        (typeStaticMethods converted ++ typeMethods converted)
        (\method -> do
          addBinding ctx (functionName method) (FunctionBinding method)
        )

      return
        [ DeclType $ d
            { typeName = if extern
              then (typeName d)
              else (modPath mod, tpName $ typeName d)
            }
        ]

    TraitDeclaration d -> do
      let name = tpName $ traitName d
      let tp   = (modPath mod, name)
      converted <- convertTraitDefinition interfaceParamConverter
                                          (d { traitName = tp })
      forM_
        (traitMethods converted)
        (\method' ->
          let method = implicitifyMethod
                vThisArgName
                (TypePtr $ TypeBasicType BasicTypeVoid)
                (\_ -> id)
                method'
          in  addBinding ctx (functionName method) (FunctionBinding method)
        )
      addToInterface ctx mod name (TraitBinding converted) False False
      return [DeclTrait d]

    ModuleVarDeclaration d -> do
      let extern = hasMeta "extern" (varMeta d)
      let name   = tpName $ varName d
      let tp     = (if extern then [] else modPath mod, name)
      converted <- convertVarDefinition interfaceConverter (d { varName = tp })
      when extern $ recordGlobalName name
      addToInterface ctx mod name (VarBinding converted) (not extern) False
      return
        [ DeclVar $ d
            { varName = if extern
              then (varName d)
              else (modPath mod, tpName $ varName d)
            }
        ]

    FunctionDeclaration d@(FunctionDefinition { functionVarargs = varargs }) ->
      do
        let
          isMain =
            functionName d == ([], "main") && (ctxMainModule ctx == modPath mod)
        let extern = (hasMeta "extern" (functionMeta d)) || isMain
        let name   = tpName $ functionName d
        let tp     = (if extern then [] else modPath mod, name)
        converted <- convertFunctionDefinition (interfaceParamConverter)
                                               (d { functionName = tp })
        when extern $ recordGlobalName name
        addToInterface ctx
                       mod
                       name
                       (FunctionBinding converted)
                       (not extern)
                       False
        return
          [ DeclFunction d
              { functionName = if extern
                then (functionName d)
                else (addNamespace (modPath mod) $ functionName d)
              }
          ]

    RuleSetDeclaration r -> do
      let name = tpName $ ruleSetName r
      let tp   = (modPath mod, name)
      r' <- convertRuleSet interfaceConverter $ r { ruleSetName = tp }
      addToInterface ctx mod name (RuleSetBinding $ r') False False
      return [DeclRuleSet $ r { ruleSetName = tp }]

    Specialize a b -> do
      modifyIORef (modSpecializations mod) (\l -> ((a, b), stmtPos s) : l)
      return []

    Implement i -> do
      let
        impl = i
          { implName = ( modPath mod
                       , s_hash
                         ( s_concat
                         $ map (s_pack . show) [implTrait i, implFor i]
                         )
                       )
          }
      return [DeclImpl impl]

    ModuleUsing using -> do
      return [DeclUsing using]

    _ -> return []
  return [ (decl, pos) | decl <- decls ]
 where
  pos = stmtPos s
  recordGlobalName name = do
    existing <- lookupBinding ctx ([], name)
    case existing of
      Just b ->
        throwk $ DuplicateGlobalNameError (modPath mod) name (bindingPos b) pos
      _ -> return ()

findImports
  :: CompileContext -> ModulePath -> [Statement] -> IO [(ModulePath, Span)]
findImports ctx mod stmts = do
  r <- foldM
    (\acc e -> case e of
      Statement { stmt = Import mp False, stmtPos = p } ->
  -- eliminate self imports (e.g. from prelude)
        return $ if mod == mp then acc else (mp, p) : acc
      Statement { stmt = Import mp True, stmtPos = p } -> do
        results <- forM (ctxSourcePaths ctx) $ \dir -> do
          let dirPath = dir </> (moduleFilePath mp -<.> "")
          exists <- doesDirectoryExist dirPath
          if not exists
            then return []
            else do
              files <- listDirectory dirPath
              return
                $ [ mp ++ [s_pack $ file -<.> ""]
                  | file <- files
                  , takeExtension file == ".kit"
                  , file /= "prelude.kit"
                  ]
        return $ [ (i, p) | i <- foldr (++) [] results ] ++ acc
      _ -> return acc
    )
    []
    stmts
  return $ reverse r

findIncludes :: [Statement] -> [(FilePath, Span)]
findIncludes stmts = foldr
  (\e acc -> case e of
    Statement { stmt = Include ip, stmtPos = p } -> (ip, p) : acc
    _ -> acc
  )
  []
  stmts
