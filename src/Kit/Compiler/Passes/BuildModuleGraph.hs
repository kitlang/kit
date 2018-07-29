module Kit.Compiler.Passes.BuildModuleGraph where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import System.Directory
import System.FilePath
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser
import Kit.Str

{-
  Starting from the compilation entry point ("main" module), recursively trace
  all imports and includes to discover the full set of modules and C modules
  that must be built. Circular imports are OK and will be processed only once.

  This step also scans modules for declarations and creates a high level
  interface which can be used in ResolveModuleTypes; we'll know e.g. that type
  X exists and is a struct, but not its fields or types, etc.
-}
buildModuleGraph :: CompileContext -> IO ()
buildModuleGraph ctx = do
  loadModule ctx (ctxMainModule ctx) Nothing
  return ()

{-
  Load a module, if it hasn't already been loaded. Also triggers recursive
  loading of the module's imports and initial type resolution of top level
  declarations.
-}
loadModule :: CompileContext -> ModulePath -> Maybe Span -> IO Module
loadModule ctx mod pos = do
  existing <- h_lookup (ctxModules ctx) mod
  case existing of
    Just x  -> return x
    Nothing -> do
      broken <- h_lookup (ctxFailedModules ctx) mod
      case broken of
        Just x -> do
          debugLog ctx
            $  "skipping known broken module <"
            ++ s_unpack (showModulePath mod)
            ++ ">"
          throwk $ KitErrors []
        Nothing -> do
          m <- _loadModule ctx mod pos
          h_insert (ctxModules ctx) mod m
          if modImports m /= []
          then
            debugLog ctx
            $  "module <"
            ++ s_unpack (showModulePath mod)
            ++ "> imports: "
            ++ (intercalate
                 ", "
                 (map (s_unpack . showModulePath . fst) $ modImports m)
               )
          else
            return ()
          forM_
            (modImports m)
            (\(mod', _) -> modifyIORef
              (ctxModuleGraph ctx)
              (\current -> ModuleGraphNode mod mod' : current)
            )
          includes <- readIORef (modIncludes m)
          forM_
            includes
            (\(mod', _) ->
              modifyIORef (ctxIncludes ctx) (\current -> mod' : current)
            )
          errs <- foldM (_loadImportedModule ctx) [] (modImports m)
          if null errs
            then return m
            else throwk $ KitErrors $ nub $ reverse errs

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
      debugLog ctx
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

_loadModule :: CompileContext -> ModulePath -> Maybe Span -> IO Module
_loadModule ctx mod pos = do
  (fp, exprs) <- parseModuleExprs ctx mod Nothing pos
  prelude     <- if last mod == "prelude"
    then return []
    else _loadPreludes ctx (take (length mod - 1) mod)
  let stmts = prelude ++ exprs
  m <- newMod mod fp
  forM_ stmts (addStmtToModuleInterface ctx m)
  let imports    = findImports mod stmts
  let includes   = findIncludes stmts
  let createdMod = m { modImports = imports }
  writeIORef (modIncludes createdMod) includes
  return createdMod

_loadImportedModule
  :: CompileContext -> [KitError] -> (ModulePath, Span) -> IO [KitError]
_loadImportedModule ctx acc (mod, pos) = do
  result <- try $ loadModule ctx mod (Just pos)
  return $ case result of
    Left  e -> e : acc
    Right m -> acc

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

addStmtToModuleInterface :: CompileContext -> Module -> Statement -> IO ()
addStmtToModuleInterface ctx mod s = do
  case stmt s of
    TypeDeclaration d@(TypeDefinition { typeName = name, typeSubtype = subtype, typeRules = rules })
      -> do
        let ct = case subtype of
              Atom       -> TypeAtom
              Struct{}   -> TypeStruct (modPath mod, name) []
              Union{}    -> TypeUnion (modPath mod, name) []
              Enum{}     -> TypeEnum (modPath mod, name) []
              Abstract{} -> TypeAbstract (modPath mod, name) []
        let extern = hasMeta "extern" (typeMeta d)
        if extern then recordGlobalName name else return ()
        addToInterface name (TypeBinding) (not extern) ct
        addDefinition
          name
          (DeclType $ d { typeNamespace = if extern then [] else modPath mod })

        subNamespace <- getSubScope (modScope mod) [name]
        -- FIXME: position
        forM_
          (typeStaticFields d)
          (\var -> do
            t <- makeTypeVar ctx (stmtPos s)
            bindToScope
              (subNamespace)
              (varName var)
              (newBinding (modPath mod ++ [name], varName var)
                          VarBinding
                          t
                          (modPath mod ++ [name])
                          (stmtPos s)
              )
          )

        case subtype of
          Enum { enumVariants = variants } -> do
            forM_
              variants
              (\variant -> do
                args <-
                  (forM
                    (variantArgs variant)
                    (\arg -> do
                      t <- makeTypeVar ctx (stmtPos s)
                      return (argName arg, t)
                    )
                  )
                addToInterface
                  (variantName variant)
                  EnumConstructor
                  False
                  (TypeEnumConstructor (modPath mod, name)
                                       (variantName variant)
                                       args
                  )
              )
          _ -> return ()
    TraitDeclaration d@(TraitDefinition { traitName = name }) -> do
      addToInterface name
                     (TraitBinding)
                     (False)
                     (TypeTraitConstraint ((modPath mod, name), []))
      addDefinition name (DeclTrait d)
    ModuleVarDeclaration d@(VarDefinition { varName = name }) -> do
      tv <- makeTypeVar ctx (stmtPos s)
      let extern = hasMeta "extern" (varMeta d)
      if extern then recordGlobalName name else return ()
      addToInterface name (VarBinding) (not extern) tv
      addDefinition
        name
        (DeclVar $ d { varNamespace = if extern then [] else modPath mod })
    FunctionDeclaration d@(FunctionDefinition { functionName = name, functionArgs = args, functionVarargs = varargs })
      -> do
        rt    <- makeTypeVar ctx (stmtPos s)
        args' <- forM
          args
          (\arg -> do
            argType <- makeTypeVar ctx (argPos arg)
            return (argName arg, argType)
          )
        let extern = hasMeta "extern" (functionMeta d)
        if extern then recordGlobalName name else return ()
        addToInterface name
                       (FunctionBinding)
                       (not extern)
                       (TypeFunction rt args' varargs)
        addDefinition
          name
          ( DeclFunction
          $ d { functionNamespace = if extern then [] else modPath mod }
          )
    Specialize a b -> do
      modifyIORef (modSpecializations mod) (\l -> ((a, b), stmtPos s) : l)
    Implement t -> do
      modifyIORef (modImpls mod) (\l -> t : l)
    _ -> return ()
 where
  pos              = stmtPos s
  recordGlobalName = addGlobalName ctx mod pos
  addToInterface name b namespace ct =
    (do
      existing <- resolveLocal (modScope mod) name
      case existing of
        Just (Binding { bindingPos = pos }) ->
          throwk $ DuplicateDeclarationError (modPath mod) name pos (stmtPos s)
        Nothing -> bindToScope
          (modScope mod)
          name
          (newBinding (modPath mod, name)
                      b
                      ct
                      (if namespace then (modPath mod) else [])
                      (stmtPos s)
          )
    )
  addDefinition name d = bindToScope (modContents mod) name d

findImports :: ModulePath -> [Statement] -> [(ModulePath, Span)]
findImports mod stmts = foldr
  (\e acc -> case e of
    Statement { stmt = Import mp, stmtPos = p } ->
      -- eliminate self imports (e.g. from prelude)
      if mod == mp then acc else (mp, p) : acc
    _ -> acc
  )
  []
  stmts

findIncludes :: [Statement] -> [(FilePath, Span)]
findIncludes stmts = foldr
  (\e acc -> case e of
    Statement { stmt = Include ip, stmtPos = p } -> (ip, p) : acc
    _ -> acc
  )
  []
  stmts
