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

data DuplicateDeclarationError = DuplicateDeclarationError ModulePath Str Span Span deriving (Eq, Show)
instance Errable DuplicateDeclarationError where
  logError e@(DuplicateDeclarationError mod name pos1 pos2) = do
    logErrorBasic e $ "Duplicate declaration for `" ++ s_unpack name ++ "` in " ++ s_unpack (showModulePath mod) ++ "; \n\nFirst declaration:"
    ePutStrLn "\nSecond declaration:"
    case file pos2 of
      Just fp -> displayFileSnippet (s_unpack fp) pos2
      _ -> return ()
    ePutStrLn "\nFunction, variable, type and trait names must be unique within a module."
  errPos (DuplicateDeclarationError _ _ pos _) = Just pos

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
          forM_
            (modIncludes m)
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
  let imports  = findImports mod stmts
  let includes = findIncludes stmts
  return m { modImports = imports, modIncludes = includes }

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
    TypeDeclaration d@(TypeDefinition { typeName = name, typeType = subtype })
      -> do
        let ct = case subtype of
              Atom       -> TypeAtom
              Struct{}   -> TypeStruct (modPath mod, name) []
              Enum{}     -> TypeEnum (modPath mod, name) []
              Abstract{} -> TypeAbstract (modPath mod, name) []
        addToInterface name (TypeBinding) ct
        addDefinition
          name
          (DefinitionType $ d { typeNameMangling = Just $ modPath mod })
    TraitDeclaration d@(TraitDefinition { traitName = name }) -> do
      addToInterface name
                     (TraitBinding)
                     (TypeTraitConstraint ((modPath mod, name), []))
      addDefinition name (DefinitionTrait d)
    ModuleVarDeclaration d@(VarDefinition { varName = name }) -> do
      tv <- makeTypeVar ctx (stmtPos s)
      addToInterface name (VarBinding) tv
      addDefinition
        name
        (DefinitionVar $ d { varNameMangling = Just $ modPath mod })
    FunctionDeclaration d@(FunctionDefinition { functionName = name, functionArgs = args, functionVarargs = varargs })
      -> do
        rt    <- makeTypeVar ctx (stmtPos s)
        args' <- forM
          args
          (\arg -> do
            -- FIXME: arg position
            argType <- makeTypeVar ctx (stmtPos s)
            return (argName arg, argType)
          )
        addToInterface name (FunctionBinding) (TypeFunction rt args' varargs)
        addDefinition
          name
          (DefinitionFunction $ d { functionNameMangling = Just $ modPath mod })
    Specialize a b -> do
      modifyIORef (modSpecializations mod) (\l -> ((a, b), stmtPos s) : l)
    Implement t -> do
      modifyIORef (modImpls mod) (\l -> t : l)
    _ -> return ()
 where
  addToInterface name b ct =
    (do
      existing <- resolveLocal (modScope mod) name
      case existing of
        Just (Binding { bindingPos = pos }) ->
          throwk $ DuplicateDeclarationError (modPath mod) name pos (stmtPos s)
        Nothing -> bindToScope
          (modScope mod)
          name
          (newBinding b ct (Just (modPath mod)) (stmtPos s))
    )
  addDefinition name d = bindToScope (modDefinitions mod) name d

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
