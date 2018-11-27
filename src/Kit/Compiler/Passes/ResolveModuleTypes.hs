module Kit.Compiler.Passes.ResolveModuleTypes where

import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.NameMangling
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.ConvertExpr
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser
import Kit.Str

data DuplicateDefaultError = DuplicateDefaultError ModulePath TypePath Span Span deriving (Eq, Show)
instance Errable DuplicateDefaultError where
  logError e@(DuplicateDefaultError mod tp pos1 pos2) = do
    logErrorBasic e $ "Duplicate default for `" ++ s_unpack (showTypePath tp) ++ "` in " ++ s_unpack (showModulePath mod) ++ "; \n\nFirst default:"
    ePutStrLn "\nSecond default:"
    displayFileSnippet pos2
    ePutStrLn "\nTraits cannot have more than one default."
  errPos (DuplicateDefaultError _ _ pos _) = Just pos

{-
  Resolving types is done in ordered passes; later passes may depend on the
  results of the previous ones.
-}
data ResolveTypesPass
  = ResolveRules
  | ResolveTypedefs
  | ResolveIdentifiers
  | ResolveImpls
  deriving (Eq, Show, Enum, Ord)

{-
  This step is responsible for actions that depend on the interfaces created
  during BuildModuleGraph, including:

  - Discovering trait implementations and defaults
  - Unifying module interface type vars with actual type annotations
-}
resolveModuleTypes
  :: CompileContext
  -> [(Module, [(Declaration Expr (Maybe TypeSpec), Span)])]
  -> IO [(Module, [TypedDeclWithContext])]
resolveModuleTypes ctx modContents = do
  results <- foldM
    (\acc step -> do
      stepResults <- forM modContents $ resolveTypesForMod step ctx
      return $ acc ++ stepResults
    )
    []
    [ResolveRules ..]
  unless (ctxIsLibrary ctx) $ validateMain ctx
  flattenDefaults ctx
  return results

flattenDefaults :: CompileContext -> IO ()
flattenDefaults ctx = do
  impls <- h_toList $ ctxImpls ctx
  memos <- h_new
  forMWithErrors_ impls $ \(tp, _) -> do
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
          forMWithErrors_ childList $ \(ct, impl) -> do
            h_insert impls ct impl
            case ct of
              TypeTraitConstraint tp -> do
                indirects    <- findImpls ctx memos tp
                indirectList <- h_toList indirects
                forMWithErrors_ indirectList $ \(ct, impl) -> do
                  h_insert impls ct impl
              _ -> return ()
          h_insert memos t impls
          return impls
        Nothing -> return impls

validateMain :: CompileContext -> IO ()
validateMain ctx = do
  main <- lookupBinding ctx ([], "main")
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
  :: ResolveTypesPass
  -> CompileContext
  -> (Module, [(Declaration Expr (Maybe TypeSpec), Span)])
  -> IO (Module, [TypedDeclWithContext])
resolveTypesForMod pass ctx (mod, contents) = do
  debugLog ctx $ show pass

  -- handle module using statements before creating final TypeContext
  tctx <- if pass > ResolveRules
    then modTypeContext ctx mod
    else newTypeContext []
  forMWithErrors_ contents $ \(decl, pos) -> do
    case decl of
      DeclUsing u -> do
        addModUsing ctx tctx mod pos u
      _ -> return ()

  when (pass == ResolveImpls) $ do
    -- resolve defaults here
    specs <- readIORef (modDefaults mod)
    forMWithErrors_ specs (addDefault ctx mod)

  let varConverter = converter (convertExpr ctx tctx mod [])
                               (resolveMaybeType ctx tctx mod [])
  let paramConverter params =
        let tctx' = addTypeParams tctx [ (p, TypeTypeParam p) | p <- params ]
        in  converter (convertExpr ctx tctx' mod params)
                      (resolveMaybeType ctx tctx' mod params)

  converted <- forM
    contents
    (\(decl, pos) -> do
      debugLog ctx
        $  "resolving types for "
        ++ (s_unpack $ showTypePath $ declName decl)
      case (pass, decl) of
        (ResolveImpls, DeclImpl (i@TraitImplementation { implFor = Just iFor, implTrait = Just trait }))
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
                  (converter (convertExpr ctx selfTctx mod [])
                             (resolveMaybeType ctx selfTctx mod [])
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

        (ResolveIdentifiers, DeclVar v) -> do
          let extern = hasMeta "extern" (varMeta v)
          converted <- convertVarDefinition varConverter
            $ v { varName = addNamespace (modPath mod) (varName v) }
          addToInterface ctx
                         mod
                         (varName converted)
                         (VarBinding converted)
                         (not extern)
                         False
          return $ Just $ (DeclVar converted, tctx)

        (ResolveIdentifiers, DeclFunction f) -> do
          let
            isMain =
              functionName f
                == ([], "main")
                && (ctxMainModule ctx == modPath mod)
          let extern = (hasMeta "extern" (functionMeta f)) || isMain
          converted <- convertFunctionDefinition paramConverter $ f
            { functionName = addNamespace (if extern then [] else modPath mod)
                                          (functionName f)
            }
          addToInterface ctx
                         mod
                         (functionName converted)
                         (FunctionBinding converted)
                         (not extern)
                         False
          return $ Just $ (DeclFunction converted, tctx)

        (ResolveIdentifiers, DeclType t) -> do
          let params' =
                [ (tp, TypeTypeParam $ tp)
                | p <- typeParams t
                , let tp = typeSubPath t $ paramName p
                ]
          let tctx' = tctx
                { tctxTypeParams = params' ++ tctxTypeParams tctx
                , tctxSelf = Just (TypeInstance (typeName t) (map snd params'))
                }
          let paramConverter params =
                let tctx'' = addTypeParams
                      tctx'
                      [ (p, TypeTypeParam p) | p <- params ]
                in  converter (convertExpr ctx tctx'' mod params)
                              (resolveMaybeType ctx tctx'' mod params)
          converted <- do
            c' <- convertTypeDefinition paramConverter
              $ t { typeName = addNamespace (modPath mod) (typeName t) }
            let c = c' { typeName = addNamespace (modPath mod) (typeName c') }
            let thisType = TypeSelf
            let m f x t = makeExprTyped x t (functionPos f)
            return $ implicitifyInstanceMethods
              thisPtrName
              (MethodTarget $ TypePtr thisType)
              (\f x -> x)
              c

          forMWithErrors_ (typeStaticFields converted)
            $ \field -> addToInterface
                ctx
                mod
                (subPath (typeName converted) $ tpName $ varName field)
                (VarBinding field)
                True
                False
          forMWithErrors_ (typeStaticMethods converted)
            $ \method -> addToInterface
                ctx
                mod
                (subPath (typeName converted) $ tpName $ functionName method)
                (FunctionBinding method)
                True
                False
          forMWithErrors_ (typeMethods converted) $ \method -> addToInterface
            ctx
            mod
            (subPath (typeName converted) $ tpName $ functionName method)
            (FunctionBinding method)
            True
            False
          case typeSubtype converted of
            Enum { enumVariants = variants } ->
              forMWithErrors_ variants $ \variant -> do
                h_insert
                  (ctxBindings ctx)
                  (subPath (typeName converted) (tpName $ variantName variant))
                  (EnumConstructor variant)
                addToInterface ctx
                               mod
                               ([], tpName $ variantName variant)
                               (EnumConstructor variant)
                               True
                               True
            _ -> return ()

          addBinding ctx (typeName t) $ TypeBinding converted
          return $ Just $ (DeclType converted, tctx')

        (ResolveIdentifiers, DeclTrait t) -> do
          let
            tctx' = tctx
              { tctxTypeParams = [ (tp, TypeTypeParam $ tp)
                                 | p <- traitAllParams t
                                 , let
                                   tp =
                                     subPath (modPath mod, tpName $ traitName t)
                                       $ paramName p
                                 ]
                ++ tctxTypeParams tctx
              , tctxSelf       = Just TypeSelf
              }
          let paramConverter params = converter
                (convertExpr ctx tctx' mod params)
                (resolveMaybeType ctx tctx' mod params)
          converted <- convertTraitDefinition paramConverter
            $ t { traitName = (modPath mod, tpName $ traitName t) }

          forMWithErrors_ (traitStaticFields converted)
            $ \field -> addToInterface
                ctx
                mod
                (subPath (traitName converted) $ tpName $ varName field)
                (VarBinding field)
                True
                False
          forMWithErrors_ (traitStaticMethods converted)
            $ \method -> addToInterface
                ctx
                mod
                (subPath (traitName converted) $ tpName $ functionName method)
                (FunctionBinding method)
                True
                False
          forMWithErrors_ (traitMethods converted) $ \method' ->
            let method = implicitifyMethod vThisArgName
                                           (MethodTarget $ TypePtr TypeVoid)
                                           (\_ -> id)
                                           method'
            in  addBinding
                  ctx
                  (subPath (traitName converted) $ tpName $ functionName method)
                  (FunctionBinding method)

          addBinding ctx (traitName converted) $ TraitBinding converted
          return $ Just $ (DeclTrait converted, tctx')

        (ResolveTypedefs, DeclTypedef a b pos) -> do
          addBinding ctx a $ TypedefBinding b (modPath mod) pos
          return Nothing

        (ResolveRules, DeclRuleSet r) -> do
          converted <- convertRuleSet varConverter r
          addBinding ctx (ruleSetName r) $ RuleSetBinding converted
          return Nothing

        _ -> return Nothing
    )

  return (mod, catMaybes converted)

addDefault :: CompileContext -> Module -> ((TypeSpec, TypeSpec), Span) -> IO ()
addDefault ctx mod ((ts@(TypeSpec tp params _), b), pos) = do
  tctx       <- modTypeContext ctx mod
  traitType  <- resolveType ctx tctx mod ts
  foundTrait <- case traitType of
    TypeTraitConstraint (tpTrait, _) -> lookupBinding ctx tpTrait
    _ -> return Nothing
  case foundTrait of
    Just (TraitBinding def) -> do
      let tp = traitName def
      existing <- h_lookup (ctxTraitDefaults ctx) tp
      case existing of
        Just (_, pos') ->
          -- if this default comes from a prelude, it could show up
          -- multiple times, so just ignore it
                          if pos' == pos
          then return ()
          else throwk $ DuplicateDefaultError (modPath mod) tp pos' pos
        _ -> do
          ct <- resolveType ctx tctx mod b
          h_insert (ctxTraitDefaults ctx) tp (ct, pos)
    _ -> throwk $ BasicError
      ("Couldn't resolve trait for default: " ++ show tp)
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
    (converter (convertExpr ctx tctx mod []) (resolveMaybeType ctx tctx mod []))
    pos
    using
  modifyIORef (modUsing mod) (\l -> converted : l)

addToInterface
  :: CompileContext
  -> Module
  -> TypePath
  -> TypedBinding
  -> Bool
  -> Bool
  -> IO ()
addToInterface ctx mod name b namespace allowCollisions = do
  unless allowCollisions $ do
    existing <- h_lookup (ctxBindings ctx) name
    case existing of
      Just x -> throwk $ DuplicateDeclarationError (modPath mod)
                                                   (tpName name)
                                                   (bindingPos x)
                                                   (bindingPos b)
      _ -> return ()
  h_insert (ctxBindings ctx) name b
