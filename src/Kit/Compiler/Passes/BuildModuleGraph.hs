module Kit.Compiler.Passes.BuildModuleGraph (buildModuleGraph, findImports) where

import Control.Exception
import Control.Monad
import Data.Mutable
import Data.List
import System.Directory
import System.FilePath
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

data ParseContext = ParseContext {
  pctxPreludes :: HashTable ModulePath [SyntacticStatement],
  pctxFailedModules :: HashTable ModulePath ()
  }

newParseContext :: IO ParseContext
newParseContext = do
  failed <- h_new
  pre    <- h_new
  return $ ParseContext {pctxPreludes = pre, pctxFailedModules = failed}

{-
  Starting from the compilation entry point ("main" module), recursively trace
  all imports and includes to discover the full set of modules and C modules
  that must be built. Circular imports are OK and will be processed only once.

  This step also scans modules for declarations and creates a high level
  interface which can be used in ResolveModuleTypes; we'll know e.g. that type
  X exists and is a struct, but not its fields or types, etc.
-}
buildModuleGraph :: CompileContext -> IO [(Module, [SyntacticStatement])]
buildModuleGraph ctx = do
  pctx    <- newParseContext
  results <- loadModule ctx pctx (ctxMainModule ctx) Nothing
  case ctxMacro ctx of
    Just (f, argSets) -> forM results $ \(mod, stmts) -> do
      if modPath mod == ctxMainModule ctx
        then do
          return
            ( mod
            , (functionDecl f) -- add the macro and a main function to invoke it
            : (functionDecl $ newFunctionDefinition
                { functionName = ([], "main")
                , functionType = InferredType (functionPos f)
                , functionPos  = functionPos f
                , functionArgs = [ newArgSpec { argName = "argc"
                                              , argType = makeTypeSpec "Int"
                                              }
                                 , newArgSpec
                                   { argName = "argv"
                                   , argType = PointerTypeSpec
                                     (makeTypeSpec "CString")
                                     NoPos
                                   }
                                 ]
                , functionBody = let p = pe (functionPos f)
                                 in
                                   Just $ p $ Block
                                     [ p $ Match
                                         (p $ Call
                                           (p $ Identifier (Var $ ([], "atoi")))
                                           []
                                           [ (p $ ArrayAccess
                                               (p $ Identifier (Var $ ([], "argv")))
                                               (p $ Literal (IntValue 1) (makeTypeSpec "Int"))
                                             )
                                           ]
                                         )
                                         [ matchCase
                                             (p $ Literal (IntValue index) (makeTypeSpec "Int"))
                                             (p
                                               (Call (p $ Identifier (Var $ functionName f))
                                                     []
                                                     args
                                               )
                                             )
                                         | (index, args) <- argSets
                                         ]
                                         Nothing
                                     ]
                }
              )
            : stmts
            )
        else return (mod, stmts)
    Nothing -> return results

{-
  Load a module, if it hasn't already been loaded. Also triggers recursive
  loading of the module's imports and initial type resolution of top level
  declarations.
-}
loadModule
  :: CompileContext
  -> ParseContext
  -> ModulePath
  -> Maybe Span
  -> IO [(Module, [SyntacticStatement])]
loadModule ctx pctx mod pos = do
  existing <- h_lookup (ctxModules ctx) mod
  case existing of
    Just x  -> return []
    Nothing -> do
      broken <- h_lookup (pctxFailedModules pctx) mod
      case broken of
        Just x -> do
          noisyDebugLog ctx
            $  "skipping known broken module <"
            ++ s_unpack (showModulePath mod)
            ++ ">"
          throwk $ KitErrors []
        Nothing -> do
          (m, decls) <- _loadModule ctx pctx mod pos
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
          includes <- readRef (modIncludes m)
          forM_
            includes
            (\(mod', _) ->
              modifyRef (ctxIncludes ctx) (\current -> mod' : current)
            )
          imports <- forMWithErrors (modImports m)
            $ _loadImportedModule ctx pctx
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
_loadPreludes
  :: CompileContext -> ParseContext -> ModulePath -> IO [SyntacticStatement]
_loadPreludes ctx pctx mod = do
  preludes <- _loadPrelude ctx pctx mod
  if null mod
    then return preludes
    else do
      _parents <- _loadPreludes ctx pctx (take (length mod - 1) mod)
      return $ _parents ++ preludes

-- Look for a single package prelude. Caches the result.
_loadPrelude
  :: CompileContext -> ParseContext -> ModulePath -> IO [SyntacticStatement]
_loadPrelude ctx pctx mod = do
  -- look for a possible prelude module for this package
  existing <- h_lookup (pctxPreludes pctx) mod
  case existing of
    Just x  -> return x
    Nothing -> do
      let preludePath = mod ++ ["prelude"]
      noisyDebugLog ctx
        $  "checking for prelude <"
        ++ s_unpack (showModulePath preludePath)
        ++ ">"
      broken <- h_exists (pctxFailedModules pctx) mod
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
              (path, preludes) <- parseModuleExprs ctx
                                                   pctx
                                                   mod
                                                   (Just fp)
                                                   Nothing
              h_insert (pctxPreludes pctx) mod preludes
              return preludes

_loadModule
  :: CompileContext
  -> ParseContext
  -> ModulePath
  -> Maybe Span
  -> IO (Module, [SyntacticStatement])
_loadModule ctx pctx mod pos = do
  (fp, exprs) <- parseModuleExprs ctx pctx mod Nothing pos
  prelude     <- if last mod == "prelude"
    then return []
    else _loadPreludes ctx pctx (take (length mod - 1) mod)
  let stmts = prelude ++ exprs
  m       <- newMod mod fp
  imports <- findImports ctx mod stmts
  let includes   = findIncludes stmts
  let linkedLibs = findLibs stmts
  modifyRef (ctxLinkedLibs ctx) (\x -> linkedLibs ++ x)
  let createdMod = m { modImports = imports }
  writeRef       (modIncludes createdMod) includes
  addModuleTypes ctx                      (modulePathToTypePath mod)
  decls <- forMWithErrors stmts (addStmtToModuleInterface ctx m)
  return (createdMod, foldr (++) [] decls)

addModuleTypes :: CompileContext -> TypePath -> IO ()
addModuleTypes ctx tp = do
  -- TODO: error on collision
  existing <- h_lookup (ctxBindings ctx) tp
  case existing of
    Just (ModuleBinding _) -> return ()
    Just x                 -> throwk $ BasicError
      ("Module name shadows a module binding: " ++ s_unpack (showTypePath tp))
      (Just $ bindingPos x)
    Nothing -> return ()
  h_insert (ctxBindings ctx) tp $ ModuleBinding tp
  when (not $ null $ tpNamespace tp) $ addModuleTypes ctx $ tpShift tp

_loadImportedModule
  :: CompileContext
  -> ParseContext
  -> (ModulePath, Span)
  -> IO (Either KitError [(Module, [SyntacticStatement])])
_loadImportedModule ctx pctx (mod, pos) =
  try $ loadModule ctx pctx mod (Just pos)

parseModuleExprs
  :: CompileContext
  -> ParseContext
  -> ModulePath
  -> Maybe FilePath
  -> Maybe Span
  -> IO (FilePath, [SyntacticStatement])
parseModuleExprs ctx pctx mod fp pos = do
  path <- case fp of
    Just fp -> do
      return fp
    Nothing -> findModule ctx mod pos
  noisyDebugLog ctx $ "parsing file " ++ show path
  parsed <- parseFile path
  case parsed of
    ParseResult r -> return (path, r)
    Err         e -> do
      h_insert (pctxFailedModules pctx) mod ()
      throwk e

findImports
  :: CompileContext
  -> ModulePath
  -> [SyntacticStatement]
  -> IO [(ModulePath, Span)]
findImports ctx mod stmts = do
  r <- foldM
    (\acc e -> case e of
      Statement { stmt = Import mp importType, stmtPos = p } ->
        case importType of
          ImportSingle ->
            -- eliminate self imports (e.g. from prelude)
            return $ if mod == mp then acc else (mp, p) : acc
          x | x == ImportWildcard || x == ImportDoubleWildcard -> do
            results <- findPackageContents ctx mp (x == ImportDoubleWildcard)
            return $ [ (i, p) | i <- results ] ++ acc
      _ -> return acc
    )
    []
    stmts
  return $ reverse r

findIncludes :: [SyntacticStatement] -> [(FilePath, Span)]
findIncludes stmts = foldr
  (\e acc -> case e of
    Statement { stmt = Include ip _, stmtPos = p } -> (ip, p) : acc
    _ -> acc
  )
  []
  stmts

findLibs :: [SyntacticStatement] -> [Str]
findLibs stmts = foldr
  (\e acc -> case e of
    Statement { stmt = Include _ (Just s), stmtPos = p } -> s : acc
    _ -> acc
  )
  []
  stmts
