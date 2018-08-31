module Kit.Compiler.Passes.ResolveModuleTypes where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Generators.NameMangling
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.ConvertExpr
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser
import Kit.Str

data DuplicateSpecializationError = DuplicateSpecializationError ModulePath TypePath Span Span deriving (Eq, Show)
instance Errable DuplicateSpecializationError where
  logError e@(DuplicateSpecializationError mod tp pos1 pos2) = do
    logErrorBasic e $ "Duplicate specialization for `" ++ s_unpack (showTypePath tp) ++ "` in " ++ s_unpack (showModulePath mod) ++ "; \n\nFirst specialization:"
    ePutStrLn "\nSecond specialization:"
    displayFileSnippet pos2
    ePutStrLn "\nTraits cannot have overlapping specializations."
  errPos (DuplicateSpecializationError _ _ pos _) = Just pos

{-
  This step is responsible for actions that depend on the interfaces created
  during BuildModuleGraph, including:

  - Discovering trait implementations and specializations
  - Unifying module interface type vars with actual type annotations
-}
resolveModuleTypes
  :: CompileContext
  -> [(Module, [(Declaration Expr (Maybe TypeSpec), Span)])]
  -> IO [(Module, [TypedDecl])]
resolveModuleTypes ctx modContents = do
  unless (ctxIsLibrary ctx) $ validateMain ctx
  results <- forM modContents $ resolveTypesForMod ctx
  flattenSpecializations ctx
  return results

flattenSpecializations :: CompileContext -> IO ()
flattenSpecializations ctx = do
  impls <- h_toList $ ctxImpls ctx
  memos <- h_new
  forM_ impls $ \(tp, _) -> do
    impls <- findImpls ctx memos tp
    h_insert (ctxImpls ctx) tp impls

findImpls
  :: CompileContext
  -> HashTable
       TraitConstraint
       (HashTable ConcreteType (TraitImplementation TypedExpr ConcreteType))
  -> TraitConstraint
  -> IO
       ( HashTable
           ConcreteType
           (TraitImplementation TypedExpr ConcreteType)
       )
findImpls ctx memos t = do
  memoized <- h_lookup memos t
  case memoized of
    Just x  -> return x
    Nothing -> do
      impls   <- h_new
      directs <- h_lookup (ctxImpls ctx) t
      case directs of
        Just x -> do
          childList <- h_toList x
          forM_ childList $ \(ct, impl) -> do
            h_insert impls ct impl
            case ct of
              TypeTraitConstraint tp -> do
                indirects    <- findImpls ctx memos tp
                indirectList <- h_toList indirects
                forM_ indirectList $ \(ct, impl) -> do
                  h_insert impls ct impl
              _ -> return ()
          h_insert memos t impls
          return impls
        Nothing -> return impls

validateMain :: CompileContext -> IO ()
validateMain ctx = do
  mod  <- getMod ctx (ctxMainModule ctx)
  main <- resolveLocal (modScope mod) "main"
  case main of
    Just (FunctionBinding f) -> do
      -- TODO
      return ()
    _ -> throwk $ BasicError
      (show mod
      ++ " doesn't have a function called 'main'; main module requires a main function"
      )
      (Nothing)

resolveTypesForMod
  :: CompileContext
  -> (Module, [(Declaration Expr (Maybe TypeSpec), Span)])
  -> IO (Module, [TypedDecl])
resolveTypesForMod ctx (mod, contents) = do
  specs <- readIORef (modSpecializations mod)
  forM_ specs (addSpecialization ctx mod)
  impls <- readIORef (modImpls mod)
  forM_ impls (addImplementation ctx mod)
  tctx <- modTypeContext ctx mod

  let varConverter =
        converter (convertExpr ctx tctx mod) (resolveMaybeType ctx tctx mod)
  let
    paramConverter params =
      let tctx' = tctx
            { tctxTypeParams = [ (p, TypeTypeParam p) | p <- params ]
              ++ tctxTypeParams tctx
            }
      in  converter (convertExpr ctx tctx' mod) (resolveMaybeType ctx tctx' mod)

  converted <- forM
    contents
    (\(decl, pos) -> do
      case decl of
        DeclUsing u -> do
          addModUsing ctx tctx mod pos u
          return Nothing
        DeclImpl i -> do
          converted <- convertTraitImplementation varConverter i
          case implTrait converted of
            TypeTraitConstraint (tp, p) -> do
              let name = subPath tp $ hashParams [implFor converted]
              let i    = converted { implName = name }
              -- correct the inferface name
              e1 <- h_get (ctxImpls ctx) (tp, p)
              e2 <- h_get e1 (implFor converted)
              h_insert e1 (implFor converted) i
              return $ Just $ DeclImpl $ i
        _ -> do
          binding <- scopeGet (modScope mod) (declName decl)
          case (binding, decl) of
            (VarBinding vi, DeclVar v) -> do
              converted <- convertVarDefinition varConverter
                $ v { varName = addNamespace (modPath mod) (varName v) }
              mergeVarInfo ctx tctx vi converted
              bindToScope (modScope mod) (declName decl) (VarBinding converted)
              return $ Just $ DeclVar converted

            (FunctionBinding fi, DeclFunction f) -> do
              let
                isMain =
                  functionName f
                    == ([], "main")
                    && (ctxMainModule ctx == modPath mod)
              let extern = (hasMeta "extern" (functionMeta f)) || isMain
              converted <- convertFunctionDefinition paramConverter $ f
                { functionName = addNamespace
                  (if extern then [] else modPath mod)
                  (functionName f)
                }
              mergeFunctionInfo ctx tctx fi converted
              bindToScope (modScope mod)
                          (declName decl)
                          (FunctionBinding converted)
              return $ Just $ DeclFunction converted

            (TypeBinding ti, DeclType t) -> do
              let params' =
                    [ (tp, TypeTypeParam $ tp)
                    | p <- typeParams t
                    , let tp = typeSubPath t $ paramName p
                    ]
              let tctx' = tctx
                    { tctxTypeParams = params' ++ tctxTypeParams tctx
                    , tctxSelf       = Just
                      (TypeInstance (typeName t) (map snd params'))
                    }
              let paramConverter params = converter
                    (convertExpr ctx tctx' mod)
                    (resolveMaybeType ctx tctx' mod)
              converted <- do
                c' <- convertTypeDefinition paramConverter
                  $ t { typeName = addNamespace (modPath mod) (typeName t) }
                let c =
                      c' { typeName = addNamespace (modPath mod) (typeName c') }
                if null (typeMethods c)
                  then return c
                  else do
                    -- thisType <- makeTypeVar ctx (typePos t)
                    let thisType = TypeSelf
                    let m f x t = makeExprTyped x t (functionPos f)
                    return $ implicitifyInstanceMethods
                      thisPtrName
                      (TypePtr thisType)
                      (\f x -> m
                        f
                        (Block
                          [ m
                            f
                            (VarDeclaration
                              (Var ([], thisArgName))
                              thisType
                              (Just $ m
                                f
                                (PreUnop
                                  Deref
                                  (m f
                                     (Identifier (Var ([], thisPtrName)))
                                     (TypePtr thisType)
                                  )
                                )
                                thisType
                              )
                            )
                            (thisType)
                          , x
                          ]
                        )
                        (inferredType x)
                      )
                      c

              forM_
                (zip (typeStaticFields ti) (typeStaticFields converted))
                (\(field1, field2) -> mergeVarInfo ctx tctx' field1 field2)
              forM_
                (zip (typeStaticMethods ti) (typeStaticMethods converted))
                (\(method1, method2) ->
                  mergeFunctionInfo ctx tctx' method1 method2
                )
              forM_
                (zip (typeMethods ti) (typeMethods converted))
                (\(method1, method2) ->
                  mergeFunctionInfo ctx tctx' method1 method2
                )
              case (typeSubtype ti, typeSubtype converted) of
                (Struct { structFields = fields1 }, Struct { structFields = fields2 })
                  -> forM_
                    (zip fields1 fields2)
                    (\(field1, field2) -> mergeVarInfo ctx tctx' field1 field2)
                (Union { unionFields = fields1 }, Union { unionFields = fields2 })
                  -> forM_
                    (zip fields1 fields2)
                    (\(field1, field2) -> mergeVarInfo ctx tctx' field1 field2)
                (Enum { enumVariants = variants1 }, Enum { enumVariants = variants2 })
                  -> forM_ (zip variants1 variants2) $ \(variant1, variant2) ->
                    do
                      forM_ (zip (variantArgs variant1) (variantArgs variant2))
                        $ \(arg1, arg2) -> mergeArgInfo ctx tctx' arg1 arg2
                      addToInterface mod
                                     (tpName $ variantName variant1)
                                     (EnumConstructor variant2)
                                     False
                                     True
                _ -> return ()

              bindToScope (modScope mod) (declName decl) (TypeBinding converted)
              return $ Just $ DeclType converted

            (TraitBinding ti, DeclTrait t) -> do
              let
                tctx' = tctx
                  { tctxTypeParams = [ (tp, TypeTypeParam $ tp)
                                     | p <- traitParams t
                                     , let tp = subPath (modPath mod, tpName $ traitName t) $ paramName p
                                     ]
                    ++ tctxTypeParams tctx
                  , tctxSelf       = Just TypeSelf
                  }
              let paramConverter params = converter
                    (convertExpr ctx tctx' mod)
                    (resolveMaybeType ctx tctx' mod)
              converted <- convertTraitDefinition paramConverter
                $ t { traitName = (modPath mod, tpName $ traitName t) }
              forM_
                (zip (traitMethods ti) (traitMethods converted))
                (\(method1, method2) ->
                  mergeFunctionInfo ctx tctx' method1 method2
                )
              bindToScope (modScope mod)
                          (declName decl)
                          (TraitBinding converted)
              return $ Just $ DeclTrait converted

            (RuleSetBinding ri, DeclRuleSet r) -> do
              -- RuleSets are untyped
              let tp = (modPath mod, tpName $ ruleSetName ri)
              bindToScope (modScope mod)
                          (declName decl)
                          (RuleSetBinding $ r { ruleSetName = tp })
              return Nothing
    )

  return (mod, catMaybes converted)

addSpecialization
  :: CompileContext -> Module -> ((TypeSpec, TypeSpec), Span) -> IO ()
addSpecialization ctx mod (((TypeSpec tp params _), b), pos) = do
  tctx  <- modTypeContext ctx mod
  found <- resolveModuleBinding ctx tctx mod tp
  case found of
    Just (TraitBinding def) -> do
      let tp = traitName def
      existing <- h_lookup (ctxTraitSpecializations ctx) tp
      case existing of
        Just (_, pos') ->
          -- if this specialization comes from a prelude, it could show up
          -- multiple times, so just ignore it
                          if pos' == pos
          then return ()
          else throwk $ DuplicateSpecializationError (modPath mod) tp pos' pos
        _ -> do
          ct <- resolveType ctx tctx mod b
          h_insert (ctxTraitSpecializations ctx) tp (ct, pos)
    _ -> throwk $ BasicError ("Couldn't resolve trait: " ++ show tp) (Just pos)

addImplementation
  :: CompileContext
  -> Module
  -> TraitImplementation Expr (Maybe TypeSpec)
  -> IO ()
addImplementation ctx mod impl@(TraitImplementation { implTrait = Just (TypeSpec tpTrait paramsTrait posTrait), implFor = Just implFor })
  = do
    tctx       <- newTypeContext []
    foundTrait <- resolveModuleBinding ctx tctx mod tpTrait
    case foundTrait of
      Just (TraitBinding def) -> do
        let tpTrait = traitName def
        paramsTrait <- mapM (resolveType ctx tctx mod) paramsTrait
        let paramTctx =
              (addTypeParams
                tctx
                [ let p = traitSubPath def $ paramName param
                  in  (p, TypeTypeParam $ p)
                | param <- traitParams def
                ]
              )
        ct <- resolveType ctx tctx mod implFor
        case ct of
          TypeTraitConstraint (tp, params) | not (null params) -> do
            makeGeneric ctx tp (implPos impl) params
            return ()
          _ -> return ()

        let selfTctx = paramTctx { tctxSelf = Just ct }
        existing <- h_lookup (ctxImpls ctx) (tpTrait, paramsTrait)
        impl     <- convertTraitImplementation
          (converter (convertExpr ctx selfTctx mod)
                     (resolveMaybeType ctx selfTctx mod)
          )
          (impl { implName = subPath (tpTrait) (tpName $ implName impl) })

        debugLog ctx
          $  "Found implementation of "
          ++ show (TypeTraitConstraint (tpTrait, paramsTrait))
          ++ " for "
          ++ show ct

        case existing of
          Just ht -> h_insert ht ct impl
          Nothing -> do
            impls <- h_new
            h_insert impls          ct                     impl
            h_insert (ctxImpls ctx) (tpTrait, paramsTrait) impls
      _ -> throwk $ BasicError ("Couldn't resolve trait: " ++ show tpTrait)
                               (Just posTrait)

addModUsing
  :: CompileContext
  -> TypeContext
  -> Module
  -> Span
  -> UsingType Expr (Maybe TypeSpec)
  -> IO ()
addModUsing ctx tctx mod pos using = do
  converted <- convertUsingType
    (converter (convertExpr ctx tctx mod) (resolveMaybeType ctx tctx mod))
    pos
    using
  modifyIORef (modUsing mod) (\l -> converted : l)

mergeVarInfo ctx tctx var1 var2 = resolveConstraint
  ctx
  tctx
  (TypeEq (varType var1)
          (varType var2)
          "Var type must match its annotation"
          (varPos var1)
  )

mergeArgInfo ctx tctx arg1 arg2 = do
  resolveConstraint
    ctx
    tctx
    (TypeEq (argType arg1)
            (argType arg2)
            "Arg type must match its annotation"
            (argPos arg1)
    )

mergeFunctionInfo ctx tctx f1 f2 = do
  resolveConstraint
    ctx
    tctx
    (TypeEq (functionType f1)
            (functionType f2)
            "Function return type must match its annotation"
            (functionPos f1)
    )
  forM (zip (functionArgs f1) (functionArgs f2))
       (\(arg1, arg2) -> mergeArgInfo ctx tctx arg1 arg2)


resolveModuleBinding
  :: CompileContext -> TypeContext -> Module -> TypePath -> IO (Maybe Binding)
resolveModuleBinding ctx tctx mod (m, name) = do
  importedMods <- getModImports ctx mod
  let searchMods = if null m
        then importedMods
        else (filter (\mod' -> modPath mod' == m) importedMods)
  resolveBinding (map modScope searchMods) name
