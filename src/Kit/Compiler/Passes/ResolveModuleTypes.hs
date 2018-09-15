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
import Kit.NameMangling
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
  -> IO [(Module, [TypedDeclWithContext])]
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
  main <- lookupBinding ctx (ctxMainModule ctx, "main")
  case main of
    Just (FunctionBinding f) -> do
      -- TODO
      return ()
    _ -> throwk $ BasicError
      (s_unpack (showModulePath $ ctxMainModule ctx)
      ++ " doesn't have a function called 'main'; main module requires a main function"
      )
      (Nothing)

resolveTypesForMod
  :: CompileContext
  -> (Module, [(Declaration Expr (Maybe TypeSpec), Span)])
  -> IO (Module, [TypedDeclWithContext])
resolveTypesForMod ctx (mod, contents) = do
  -- handle module using statements before creating final TypeContext
  tctx <- modTypeContext ctx mod
  forM_ contents $ \(decl, pos) -> do
    case decl of
      DeclUsing u -> do
        addModUsing ctx tctx mod pos u
      _ -> return ()

  specs <- readIORef (modSpecializations mod)
  forM_ specs (addSpecialization ctx mod)
  tctx <- modTypeContext ctx mod

  let varConverter =
        converter (convertExpr ctx tctx mod) (resolveMaybeType ctx tctx mod)
  let paramConverter params =
        let tctx' = addTypeParams tctx [ (p, TypeTypeParam p) | p <- params ]
        in  converter (convertExpr ctx tctx' mod)
                      (resolveMaybeType ctx tctx' mod)

  converted <- forM
    contents
    (\(decl, pos) -> do
      case decl of
        DeclUsing u -> do
          return Nothing

        DeclImpl (i@TraitImplementation { implFor = Just iFor, implTrait = Just trait })
          -> do
            traitCt <- resolveType ctx tctx mod trait
            case traitCt of
              TypeTraitConstraint (tpTrait, paramsTrait) -> do
                paramsTrait <- return $ take
                  (length paramsTrait - (length $ implAssocTypes i))
                  paramsTrait
                def <- getTraitDefinition ctx tpTrait

                let paramTctx = addTypeParams
                      tctx
                      [ let p = traitSubPath def $ paramName param
                        in  (p, TypeTypeParam $ p)
                      | param <- traitAllParams def
                      ]
                ct <- resolveType ctx paramTctx mod iFor
                let selfTctx = paramTctx { tctxSelf = Just ct }
                impl <- convertTraitImplementation
                  (converter (convertExpr ctx selfTctx mod)
                             (resolveMaybeType ctx selfTctx mod)
                  )
                  (i { implName = subPath (tpTrait) (tpName $ implName i) })

                useImpl ctx tctx (implPos impl) def impl paramsTrait
                let name = subPath tpTrait $ hashParams [implFor impl]

                -- correct the inferface name
                let key  = (tpTrait, paramsTrait)
                e1 <- h_lookup (ctxImpls ctx) key
                e1 <- case e1 of
                  Just x  -> return x
                  Nothing -> do
                    x <- h_new
                    h_insert (ctxImpls ctx) key x
                    return x
                h_insert e1 (implFor impl) impl

                let assocParams = implAssocTypes impl

                case ct of
                  TypeTraitConstraint (tp, params) | not (null params) -> do
                    -- if implementing trait for trait, create the monomorph
                    makeGeneric ctx tp (implPos impl) (params ++ assocParams)
                    return ()
                  _ -> return ()
                existing <- h_lookup (ctxImpls ctx) (tpTrait, paramsTrait)

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

                return $ Just $ (DeclImpl $ impl, tctx)

              _ -> throwk $ BasicError
                (  "Couldn't resolve trait for trait implementation: "
                ++ (show $ implTrait i)
                )
                (Just $ implPos i)

        _ -> do
          binding <- getBinding ctx (modPath mod, declName decl)
          case (binding, decl) of
            (VarBinding vi, DeclVar v) -> do
              converted <- convertVarDefinition varConverter
                $ v { varName = addNamespace (modPath mod) (varName v) }
              mergeVarInfo ctx
                           tctx
                           vi
                           converted
                           "Variable type must match its annotation"
              addBinding ctx (varName v) $ VarBinding converted
              return $ Just $ (DeclVar converted, tctx)

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
              mergeFunctionInfo
                ctx
                tctx
                fi
                converted
                "Function return type must match its annotation"
                "Function argument type must match its annotation"
              addBinding ctx (functionName f) $ FunctionBinding converted
              return $ Just $ (DeclFunction converted, tctx)

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
              let
                paramConverter params =
                  let
                    tctx'' =
                      addTypeParams tctx' [ (p, TypeTypeParam p) | p <- params ]
                  in  converter (convertExpr ctx tctx'' mod)
                                (resolveMaybeType ctx tctx'' mod)
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
                    return $ implicitifyInstanceMethods thisPtrName
                                                        (TypePtr thisType)
                                                        (\f x -> x)
                                                        c

              forM_
                (zip (typeStaticFields ti) (typeStaticFields converted))
                (\(field1, field2) -> mergeVarInfo
                  ctx
                  tctx'
                  field1
                  field2
                  "Static field type must match its annotation"
                )
              forM_
                (zip (typeStaticMethods ti) (typeStaticMethods converted))
                (\(method1, method2) -> mergeFunctionInfo
                  ctx
                  tctx'
                  method1
                  method2
                  "Static method return type must match its annotation"
                  "Static method argument type must match its annotation"
                )
              forM_
                (zip (typeMethods ti) (typeMethods converted))
                (\(method1, method2) -> mergeFunctionInfo
                  ctx
                  tctx'
                  method1
                  method2
                  "Method return type must match its annotation"
                  "Method argument type must match its annotation"
                )
              case (typeSubtype ti, typeSubtype converted) of
                (Struct { structFields = fields1 }, Struct { structFields = fields2 })
                  -> forM_
                    (zip fields1 fields2)
                    (\(field1, field2) -> mergeVarInfo
                      ctx
                      tctx'
                      field1
                      field2
                      "Struct field type must match its annotation"
                    )
                (Union { unionFields = fields1 }, Union { unionFields = fields2 })
                  -> forM_
                    (zip fields1 fields2)
                    (\(field1, field2) -> mergeVarInfo
                      ctx
                      tctx'
                      field1
                      field2
                      "Union field type must match its annotation"
                    )
                (Enum { enumVariants = variants1 }, Enum { enumVariants = variants2 })
                  -> forM_ (zip variants1 variants2) $ \(variant1, variant2) ->
                    do
                      forM_ (zip (variantArgs variant1) (variantArgs variant2))
                        $ \(arg1, arg2) -> mergeArgInfo
                            ctx
                            tctx'
                            arg1
                            arg2
                            "Enum constructor argument type must match its annotation"
                      addToInterface ctx
                                     mod
                                     (tpName $ variantName variant1)
                                     (EnumConstructor variant2)
                                     False
                                     True
                _ -> return ()

              addBinding ctx (typeName t) $ TypeBinding converted
              return $ Just $ (DeclType converted, tctx')

            (TraitBinding ti, DeclTrait t) -> do
              let
                tctx' = tctx
                  { tctxTypeParams = [ (tp, TypeTypeParam $ tp)
                                     | p <- traitAllParams t
                                     , let
                                       tp =
                                         subPath
                                             (modPath mod, tpName $ traitName t)
                                           $ paramName p
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
                (\(method1, method2) -> mergeFunctionInfo
                  ctx
                  tctx'
                  method1
                  method2
                  "Trait method return type must match its annotation"
                  "Trait method argument type must match its annotation"
                )
              addBinding ctx (traitName t) $ TraitBinding converted
              return $ Just $ (DeclTrait converted, tctx')

            (RuleSetBinding ri, DeclRuleSet r) -> do
              -- RuleSets are untyped
              let tp = (modPath mod, tpName $ ruleSetName ri)
              addBinding ctx (ruleSetName r) $ RuleSetBinding $ r
                { ruleSetName = tp
                }
              return Nothing
    )

  return (mod, catMaybes converted)

addSpecialization
  :: CompileContext -> Module -> ((TypeSpec, TypeSpec), Span) -> IO ()
addSpecialization ctx mod ((ts@(TypeSpec tp params _), b), pos) = do
  tctx       <- modTypeContext ctx mod
  traitType  <- resolveType ctx tctx mod ts
  foundTrait <- case traitType of
    TypeTraitConstraint (tpTrait, _) -> lookupBinding ctx tpTrait
    _ -> return Nothing
  case foundTrait of
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
    _ -> throwk $ BasicError
      ("Couldn't resolve trait for specialization: " ++ show tp)
      (Just pos)

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
