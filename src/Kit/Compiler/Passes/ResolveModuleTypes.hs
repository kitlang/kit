module Kit.Compiler.Passes.ResolveModuleTypes (resolveModuleTypes) where

import Control.Monad
import Data.Mutable
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.NameMangling
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
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
  | ResolveExtensions
  | ResolveTypes
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
  -> [(Module, [SyntacticStatement])]
  -> IO [(Module, [TypedStmtWithContext])]
resolveModuleTypes ctx modContents = do
  extensions <- h_new
  results    <- foldM
    (\acc step -> do
      stepResults <- forM modContents $ resolveTypesForMod step ctx extensions
      return $ acc ++ stepResults
    )
    []
    [ResolveRules ..]
  unless (ctxIsLibrary ctx) $ validateMain ctx
  return results

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
  -> HashTable TypePath (IORef [DefStatement Expr TypeSpec])
  -> (Module, [SyntacticStatement])
  -> IO (Module, [TypedStmtWithContext])
resolveTypesForMod pass ctx extensions (mod, contents) = do
  -- handle module using statements before creating final TypeContext
  tctx <- if pass > ResolveRules
    then modTypeContext ctx mod
    else newTypeContext []
  forMWithErrors_ contents $ \decl -> do
    case stmt decl of
      ModuleUsing u -> do
        addModUsing ctx tctx mod (stmtPos decl) u
      _ -> return ()

  when (pass == ResolveImpls) $ do
    -- resolve defaults here
    specs <- readRef (modDefaults mod)
    forMWithErrors_ specs (addDefault ctx mod)

  let varConverter = converter (convertExpr ctx tctx mod [])
                               (resolveMaybeType ctx tctx mod [])
  let paramConverter pos params = do
        tctx <- addTypeParams ctx
                              tctx
                              [ (p, TypeTypeParam p) | p <- params ]
                              pos
        return $ converter (convertExpr ctx tctx mod params)
                           (resolveMaybeType ctx tctx mod params)

  converted <- forM
    contents
    (\decl -> do
      noisyDebugLog ctx $ "resolving types for " ++ (show $ stmt decl)
      case (pass, stmt decl) of
        (ResolveImpls, Implement (i@TraitImplementation { implFor = iFor, implTrait = trait }))
          -> do
            traitCt <- resolveType ctx tctx mod trait
            case traitCt of
              TypeTraitConstraint (tpTrait, paramsTrait) -> do
                paramsTrait <- return $ take
                  (length paramsTrait - (length $ implAssocTypes i))
                  paramsTrait
                def       <- getTraitDefinition ctx tpTrait

                paramTctx <- addTypeParams
                  ctx
                  tctx
                  [ let p = traitSubPath def $ paramName param
                    in  (p, TypeTypeParam $ p)
                  | param <- traitAllParams def
                  ]
                  (implPos i)
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

                return $ Just $ (ps (implPos impl) $ Implement impl, tctx)

              _ -> throwk $ BasicError
                (  "Couldn't resolve trait for trait implementation: "
                ++ (show $ implTrait i)
                )
                (Just $ implPos i)

        (ResolveIdentifiers, VarDeclaration v) -> do
          let extern = hasMeta "extern" (varMeta v)
          converted <- convertVarDefinition varConverter
            $ v { varName = addNamespace (modPath mod) (varName v) }
          addToInterface ctx
                         mod
                         (varName converted)
                         (VarBinding converted)
                         (not extern)
                         False
          return $ Just $ (ps (varPos v) $ VarDeclaration converted, tctx)

        (ResolveIdentifiers, FunctionDeclaration f) -> do
          let
            isMain =
              functionName f
                == ([], "main")
                && (ctxMainModule ctx == modPath mod)
          let extern = (hasMeta "extern" (functionMeta f)) || isMain
          converted <-
            convertFunctionDefinition (paramConverter $ functionPos f) $ f
              { functionName = addNamespace
                (if extern then [] else modPath mod)
                (functionName f)
              }
          addToInterface ctx
                         mod
                         (functionName converted)
                         (FunctionBinding converted)
                         (not extern)
                         False
          return
            $ Just
            $ (ps (functionPos f) $ FunctionDeclaration converted, tctx)

        (ResolveExtensions, ExtendDefinition t stmts) -> do
          let pos = case t of
                InferredType pos -> pos
                _                -> position t
          ct <- resolveMaybeType ctx tctx mod [] pos t
          let tp = case ct of
                TypeInstance tp _           -> tp
                TypeTraitConstraint (tp, _) -> tp
                _                           -> throwk $ BasicError
                  ("Couldn't resolve extension target for type " ++ show t)
                  (Just pos)
          existing <- h_lookup extensions tp
          ref      <- case existing of
            Just x  -> return x
            Nothing -> do
              new <- newRef []
              h_insert extensions tp new
              return new
          modifyRef ref (\x -> x ++ stmts)
          return Nothing

        (ResolveTypes, TypeDeclaration t) -> do
          extensions <- h_lookup extensions $ typeName t
          extensions <- case extensions of
            Just x  -> readRef x
            Nothing -> return []
          t <- return $ foldr addTypeExtension t extensions
          let params' =
                [ (tp, TypeTypeParam $ tp)
                | p <- typeParams t
                , let tp = typeSubPath t $ paramName p
                ]
          tctx <- return $ tctx
            { tctxTypeParams = params' ++ tctxTypeParams tctx
            , tctxSelf = Just (TypeInstance (typeName t) (map snd params'))
            }
          let paramConverter params = do
                tctx <- addTypeParams ctx
                                      tctx
                                      [ (p, TypeTypeParam p) | p <- params ]
                                      (typePos t)
                return $ converter (convertExpr ctx tctx mod params)
                                   (resolveMaybeType ctx tctx mod params)
          converted <- do
            c <- convertTypeDefinition paramConverter t
            let thisType = TypeSelf
            let m f x t = makeExprTyped x t (functionPos f)
            return $ implicitifyInstanceMethods
              thisPtrName
              (MethodTarget $ TypePtr thisType)
              (\f x -> x)
              c

          forMWithErrors_ (typeStaticFields converted)
            $ \field -> addToInterface ctx
                                       mod
                                       (varName field)
                                       (VarBinding field)
                                       True
                                       False
          forMWithErrors_ (typeStaticMethods converted) $ \method ->
            addToInterface ctx
                           mod
                           (functionName method)
                           (FunctionBinding method)
                           True
                           False
          forMWithErrors_ (typeMethods converted) $ \method -> addToInterface
            ctx
            mod
            (functionName method)
            (FunctionBinding method)
            True
            False
          case typeSubtype converted of
            Enum { enumVariants = variants } -> do
              forMWithErrors_ variants $ \variant -> do
                forM_
                    [ (subPath (typeName converted)
                               (tpName $ variantName variant)
                      )
                    , (modPath mod, (tpName $ variantName variant))
                    ]
                  $ \name -> h_insert
                      (ctxBindings ctx)
                      name
                      (EnumConstructor variant)
                when (enumIsSimple $ typeSubtype converted) $ h_insert
                  (ctxBindings ctx)
                  (subPath (typeName converted) "variants")
                  (ExprBinding $ makeExprTyped
                    (ArrayLiteral
                      [ makeExprTyped (Identifier $ Var $ variantName variant)
                                      (TypeInstance (typeName converted) [])
                                      (typePos converted)
                      | variant <- variants
                      ]
                    )
                    (TypeArray (TypeInstance (typeName converted) [])
                               (length variants)
                    )
                    (typePos converted)
                  )
            StructUnion { structUnionFields = fields } -> do
              h_insert
                (ctxBindings ctx)
                (subPath (typeName converted) "fields")
                (ExprBinding $ makeExprTyped
                  (ArrayLiteral
                    [ makeExprTyped
                        (Literal (StringValue $ tpName $ varName field)
                                 TypeCString
                        )
                        TypeCString
                        (typePos converted)
                    | field <- fields
                    ]
                  )
                  (TypeArray TypeCString (length fields))
                  (typePos converted)
                )
            _ -> return ()

          addBinding ctx (typeName t) $ TypeBinding converted
          return
            $ Just
            $ (ps (typePos converted) $ TypeDeclaration converted, tctx)

        (ResolveTypes, TraitDeclaration t) -> do
          extensions <- h_lookup extensions $ traitName t
          extensions <- case extensions of
            Just x  -> readRef x
            Nothing -> return []
          t <- return $ foldr addTraitExtension t extensions
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
          let paramConverter params = return $ converter
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
          return
            $ Just
            $ (ps (traitPos converted) $ TraitDeclaration converted, tctx')

        (ResolveTypedefs, Typedef a b) -> do
          addBinding ctx a $ TypedefBinding b (modPath mod) (stmtPos decl)
          return Nothing

        (ResolveRules, RuleSetDeclaration r) -> do
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
  -> UsingType Expr TypeSpec
  -> IO ()
addModUsing ctx tctx mod pos using = do
  converted <- convertUsingType
    (converter (convertExpr ctx tctx mod []) (resolveMaybeType ctx tctx mod []))
    pos
    using
  modifyRef (modUsing mod) (\l -> converted : l)

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
      Just (FunctionBinding (FunctionDefinition { functionIsImplemented = False }))
        -> return ()
      Just x -> throwk $ DuplicateDeclarationError (modPath mod)
                                                   (tpName name)
                                                   (bindingPos x)
                                                   (bindingPos b)
      _ -> return ()
  h_insert (ctxBindings ctx) name b
