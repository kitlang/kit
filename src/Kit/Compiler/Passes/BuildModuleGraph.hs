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
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser
import Kit.Str

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
          imports <- forM (modImports m) (_loadImportedModule ctx)
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
  m <- newMod mod fp
  let imports    = findImports mod stmts
  let includes   = findIncludes stmts
  let createdMod = m { modImports = imports }
  writeIORef (modIncludes createdMod) includes
  decls <- forM stmts (addStmtToModuleInterface ctx m)
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

addStmtToModuleInterface
  :: CompileContext -> Module -> Statement -> IO [(SyntacticDecl, Span)]
addStmtToModuleInterface ctx mod s = do
  -- the expressions from these conversions shouldn't be used;
  -- we'll use the actual typed versions generated later
  let interfaceConverter = converter
        (\e -> do
          tv <- makeTypeVar ctx (ePos e)
          return $ makeExprTyped (This) tv (ePos e)
        )
        (\pos _ -> do
          makeTypeVar ctx pos
        )
  decls <- case stmt s of
    TypeDeclaration d@(TypeDefinition { typeName = name, typeSubtype = subtype, typeRules = rules })
      -> do
        let ct = TypeInstance (modPath mod, name) []
              -- [TypeTypeParam (paramName p) | p <- typeParams d ]

        converted <- do
          c <- convertTypeDefinition (\_ -> interfaceConverter) d
          if null (typeMethods d)
            then return c
            else do
              return $ implicitifyInstanceMethods ct c

        let b      = TypeBinding converted
        let extern = hasMeta "extern" (typeMeta d)
        when extern $ recordGlobalName name
        addToInterface mod name b (not extern) ct pos False

        subNamespace <- getSubScope (modScope mod) [name]
        forM_
          (typeStaticFields converted)
          (\field -> do
            bindToScope
              (subNamespace)
              (varName field)
              (newBinding (modPath mod ++ [name], varName field)
                          (VarBinding field)
                          (varType field)
                          (modPath mod ++ [name])
                          (varPos field)
              )
          )
        forM_
          (typeStaticMethods converted ++ typeMethods converted)
          (\method -> do
            bindToScope
              (subNamespace)
              (functionName method)
              (newBinding
                (modPath mod ++ [name], functionName method)
                (FunctionBinding method)
                (TypeFunction
                  (functionType method)
                  [ (argName arg, argType arg) | arg <- functionArgs method ]
                  (functionVarargs method)
                  []
                )
                (modPath mod ++ [name])
                (functionPos method)
              )
          )

        return
          [DeclType $ d { typeNamespace = if extern then [] else modPath mod }]

    TraitDeclaration d@(TraitDefinition { traitName = name }) -> do
      subNamespace <- getSubScope (modScope mod) [name]
      converted    <- convertTraitDefinition (\_ -> interfaceConverter) d
      forM_
        (traitMethods converted)
        (\method' ->
          let method = implicitifyMethod
                (TypePtr $ TypeBasicType BasicTypeVoid)
                (vThisArgName)
                method'
          in
            bindToScope
              (subNamespace)
              (functionName method)
              (newBinding
                (modPath mod ++ [name], functionName method)
                (FunctionBinding method)
                (TypeFunction
                  (functionType method)
                  [ (argName arg, argType arg) | arg <- functionArgs method ]
                  (functionVarargs method)
                  []
                )
                (modPath mod ++ [name])
                (functionPos method)
              )
        )
      addToInterface mod
                     name
                     (TraitBinding converted)
                     (False)
                     (TypeTraitConstraint ((modPath mod, name), []))
                     pos
                     False
      return [DeclTrait d]

    ModuleVarDeclaration d@(VarDefinition { varName = name }) -> do
      converted <- convertVarDefinition interfaceConverter d
      let extern = hasMeta "extern" (varMeta d)
      when extern $ recordGlobalName name
      addToInterface mod
                     name
                     (VarBinding converted)
                     (not extern)
                     (varType converted)
                     pos
                     False
      return [DeclVar $ d { varNamespace = if extern then [] else modPath mod }]

    FunctionDeclaration d@(FunctionDefinition { functionName = name, functionArgs = args, functionVarargs = varargs })
      -> do
        converted <- convertFunctionDefinition (\_ -> interfaceConverter) d
        let extern = hasMeta "extern" (functionMeta d)
        when extern $ recordGlobalName name
        addToInterface
          mod
          name
          (FunctionBinding converted)
          (not extern)
          (TypeFunction
            (functionType converted)
            [ (argName arg, argType arg) | arg <- functionArgs converted ]
            varargs
            []
          )
          pos
          False
        return
          [ DeclFunction d
              { functionNamespace = if extern then [] else modPath mod
              }
          ]

    RuleSetDeclaration r -> do
      addToInterface mod
                     (ruleSetName r)
                     (RuleSetBinding r)
                     (False)
                     (TypeRuleSet (modPath mod, ruleSetName r))
                     pos
                     False
      return [DeclRuleSet r]

    Specialize a b -> do
      modifyIORef (modSpecializations mod) (\l -> ((a, b), stmtPos s) : l)
      return []

    Implement i -> do
      modifyIORef (modImpls mod) (\l -> i : l)
      return [DeclImpl (i { implMod = modPath mod })]

    ModuleUsing using -> do
      return [DeclUsing using]

    _ -> return []
  return [ (decl, pos) | decl <- decls ]
 where
  pos              = stmtPos s
  recordGlobalName = addGlobalName ctx mod pos

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
