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
  -> [(Module, [Declaration Expr (Maybe TypeSpec)])]
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
       TypePath
       (HashTable ConcreteType (TraitImplementation TypedExpr ConcreteType))
  -> TypePath
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
              TypeTraitConstraint (tp, params) -> do
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
    Just (Binding { bindingType = FunctionBinding f }) -> do
      -- TODO
      return ()
    _ -> throwk $ BasicError
      (show mod
      ++ " doesn't have a function called 'main'; main module requires a main function"
      )
      (Nothing)

resolveTypesForMod
  :: CompileContext
  -> (Module, [Declaration Expr (Maybe TypeSpec)])
  -> IO (Module, [TypedDecl])
resolveTypesForMod ctx (mod, contents) = do
  specs <- readIORef (modSpecializations mod)
  forM_ specs (addSpecialization ctx mod)
  impls <- readIORef (modImpls mod)
  forM_ impls (addImplementation ctx mod)
  tctx <- modTypeContext ctx mod

  let varConverter =
        converter (convertExpr ctx tctx mod) (resolveMaybeType ctx tctx mod)
  -- TODO: params
  let
    paramConverter params =
      let tctx' = tctx
            { tctxTypeParams = [ (p, TypeTypeParam p) | p <- params ]
              ++ tctxTypeParams tctx
            }
      in  converter (convertExpr ctx tctx' mod) (resolveMaybeType ctx tctx' mod)

  converted <- forM
    contents
    (\decl -> do
      case decl of
        DeclUsing u -> do
          addModUsing ctx tctx mod u
          return Nothing
        DeclImpl i -> do
          converted <- convertTraitImplementation varConverter i
          return $ Just $ DeclImpl converted
        _ -> do
          binding <- scopeGet (modScope mod) (declName decl)
          case (bindingType binding, decl) of
            (VarBinding vi, DeclVar v) -> do
              converted <- convertVarDefinition varConverter v
              mergeVarInfo ctx tctx vi converted
              bindToScope (modScope mod)
                          (declName decl)
                          (binding { bindingType = VarBinding converted })
              return $ Just $ DeclVar converted

            (FunctionBinding fi, DeclFunction f) -> do
              converted <- convertFunctionDefinition paramConverter f
              mergeFunctionInfo ctx tctx fi converted
              bindToScope
                (modScope mod)
                (declName decl)
                (binding { bindingType = FunctionBinding converted })
              return $ Just $ DeclFunction converted

            (TypeBinding ti, DeclType t) -> do
              let
                paramConverter params =
                  let
                    tctx' = tctx
                      { tctxTypeParams = [ (p, TypeTypeParam p) | p <- params ]
                        ++ tctxTypeParams tctx
                      , tctxSelf       = Just (modPath mod, typeName t)
                      }
                  in  converter (convertExpr ctx tctx' mod)
                                (resolveMaybeType ctx tctx' mod)
              converted <- do
                c <- convertTypeDefinition paramConverter t
                if null (typeMethods c)
                  then return c
                  else do
                    thisType <- makeTypeVar ctx (typePos t)
                    return $ implicitifyInstanceMethods thisType c

              forM_ (zip (typeStaticFields ti) (typeStaticFields converted))
                    (\(field1, field2) -> mergeVarInfo ctx tctx field1 field2)
              forM_
                (zip (typeStaticMethods ti) (typeStaticMethods converted))
                (\(method1, method2) ->
                  mergeFunctionInfo ctx tctx method1 method2
                )
              forM_
                (zip (typeMethods ti) (typeMethods converted))
                (\(method1, method2) ->
                  mergeFunctionInfo ctx tctx method1 method2
                )
              case (typeSubtype ti, typeSubtype converted) of
                (Struct { structFields = fields1 }, Struct { structFields = fields2 })
                  -> forM_
                    (zip fields1 fields2)
                    (\(field1, field2) -> mergeVarInfo ctx tctx field1 field2)
                (Union { unionFields = fields1 }, Union { unionFields = fields2 })
                  -> forM_
                    (zip fields1 fields2)
                    (\(field1, field2) -> mergeVarInfo ctx tctx field1 field2)
                _ -> return ()

              bindToScope (modScope mod)
                          (declName decl)
                          (binding { bindingType = TypeBinding converted })
              return $ Just $ DeclType converted

            (TraitBinding ti, DeclTrait t) -> do
              converted <- convertTraitDefinition paramConverter t
              forM_
                (zip (traitMethods ti) (traitMethods converted))
                (\(method1, method2) ->
                  mergeFunctionInfo ctx tctx method1 method2
                )
              bindToScope (modScope mod)
                          (declName decl)
                          (binding { bindingType = TraitBinding converted })
              return $ Just $ DeclTrait converted

            (RuleSetBinding ri, DeclRuleSet r) -> do
              -- RuleSets are untyped
              bindToScope (modScope mod)
                          (declName decl)
                          (binding { bindingType = RuleSetBinding r })
              return Nothing
    )

  return (mod, catMaybes converted)

addSpecialization
  :: CompileContext -> Module -> ((TypeSpec, TypeSpec), Span) -> IO ()
addSpecialization ctx mod (((TypeSpec tp params _), b), pos) = do
  tctx  <- modTypeContext ctx mod
  found <- resolveModuleBinding ctx tctx mod tp
  case found of
    Just (Binding { bindingType = TraitBinding _, bindingConcrete = TypeTraitConstraint (tp, params') })
      -> do
      -- TODO: params
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
    foundTrait <- resolveModuleBinding ctx tctx mod (tpTrait)
    case foundTrait of
      Just (Binding { bindingType = TraitBinding _, bindingConcrete = TypeTraitConstraint (tpTrait, tpParams) })
        -> do
          ct       <- resolveType ctx tctx mod implFor
          existing <- h_lookup (ctxImpls ctx) tpTrait
          impl'    <- convertTraitImplementation
            (converter (convertExpr ctx tctx mod)
                       (resolveMaybeType ctx tctx mod)
            )
            impl
          let impl = impl' { implMod = modPath mod }

          case existing of
            Just ht -> h_insert ht ct impl
            Nothing -> do
              impls <- h_new
              h_insert impls          ct      impl
              h_insert (ctxImpls ctx) tpTrait impls
      _ -> throwk $ BasicError ("Couldn't resolve trait: " ++ show tpTrait)
                               (Just posTrait)

addModUsing
  :: CompileContext
  -> TypeContext
  -> Module
  -> UsingType Expr (Maybe TypeSpec)
  -> IO ()
addModUsing ctx tctx mod using = do
  converted <- convertUsingType
    (converter (convertExpr ctx tctx mod) (resolveMaybeType ctx tctx mod))
    NoPos
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

mergeFunctionInfo ctx tctx f1 f2 = do
  resolveConstraint
    ctx
    tctx
    (TypeEq (functionType f1)
            (functionType f2)
            "Function return type must match its annotation"
            (functionPos f1)
    )
  forM
    (zip (functionArgs f1) (functionArgs f2))
    (\(arg1, arg2) -> do
      resolveConstraint
        ctx
        tctx
        (TypeEq (argType arg1)
                (argType arg2)
                "Function argument type must match its annotation"
                (argPos arg1)
        )
    )


resolveModuleBinding
  :: CompileContext -> TypeContext -> Module -> TypePath -> IO (Maybe Binding)
resolveModuleBinding ctx tctx mod (m, name) = do
  importedMods <- getModImports ctx mod
  let searchMods = if null m
        then importedMods
        else (filter (\mod' -> modPath mod' == m) importedMods)
  resolveBinding (map modScope searchMods) name
