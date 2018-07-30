module Kit.Compiler.Passes.ResolveModuleTypes where

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
  - Resolving type annotations to specific types
-}
resolveModuleTypes :: CompileContext -> IO ()
resolveModuleTypes ctx = do
  mods <- h_toList $ ctxModules ctx
  forM_ (map snd mods) (resolveTypesForMod ctx)
  if (ctxIsLibrary ctx) then return () else validateMain ctx
  return ()

validateMain :: CompileContext -> IO ()
validateMain ctx = do
  mod  <- getMod ctx (ctxMainModule ctx)
  main <- resolveLocal (modScope mod) "main"
  case main of
    Just (Binding { bindingType = FunctionBinding }) -> return ()
    _ -> throwk $ BasicError
      (show mod
      ++ " doesn't have a function called 'main'; main module requires a main function"
      )
      (Nothing)

resolveTypesForMod :: CompileContext -> Module -> IO ()
resolveTypesForMod ctx mod = do
  specs <- readIORef (modSpecializations mod)
  forM_ specs (addSpecialization ctx mod)
  impls <- readIORef (modImpls mod)
  forM_ impls (addImplementation ctx mod)
  contents <- bindingList (modContents mod)
  forM_ contents (resolveDecl ctx mod)
  return ()

addSpecialization
  :: CompileContext -> Module -> ((TypeSpec, TypeSpec), Span) -> IO ()
addSpecialization ctx mod (((TypeSpec tp params _), b), pos) = do
  tctx  <- newTypeContext []
  found <- resolveModuleBinding ctx tctx mod tp
  case found of
    Just (Binding { bindingType = TraitBinding, bindingConcrete = TypeTraitConstraint (tp, params') })
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
          _ -> h_insert (ctxTraitSpecializations ctx) tp (b, pos)
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
      Just (Binding { bindingType = TraitBinding, bindingConcrete = TypeTraitConstraint (tpTrait, tpParams) })
        -> do
          ct       <- resolveType ctx tctx mod implFor
          existing <- h_lookup (ctxImpls ctx) tpTrait
          case existing of
            Just ht -> h_insert ht ct impl
            Nothing -> do
              impls <- h_new
              h_insert impls          ct      impl
              h_insert (ctxImpls ctx) tpTrait impls
      _ -> throwk $ BasicError ("Couldn't resolve trait: " ++ show tpTrait)
                               (Just posTrait)

resolveDecl :: CompileContext -> Module -> Decl -> IO ()
resolveDecl ctx mod decl = case decl of
  DeclVar v@(VarDefinition { varName = name, varType = Just t }) -> do
    tctx    <- newTypeContext []
    modType <- resolveLocal (modScope mod) name
    case modType of
      Just (Binding { bindingConcrete = ct }) -> do
        t' <- resolveType ctx tctx mod t
        resolveConstraint
          ctx
          tctx
          mod
          (TypeEq ct
                  t'
                  "Var type must match its annotation"
                  (typeSpecPosition t)
          )
      _ -> throwk $ InternalError
        ("Unexpected missing var binding " ++ s_unpack name)
        Nothing
  DeclFunction f@(FunctionDefinition { functionName = name, functionArgs = args, functionType = rt })
    -> do
    -- TODO: params
      tctx    <- newTypeContext []
      modType <- resolveLocal (modScope mod) name
      case modType of
        Just (Binding { bindingConcrete = TypeFunction cRt cArgs _ }) -> do
          case rt of
            Just rt -> do
              rt' <- resolveType ctx tctx mod rt
              resolveConstraint
                ctx
                tctx
                mod
                (TypeEq cRt
                        rt'
                        "Function return type must match its annotation"
                        (typeSpecPosition rt)
                )
            _ -> return ()
          forM_
            (zip args cArgs)
            (\(arg, (_, cArgType)) -> case argType arg of
              Just t -> do
                t' <- resolveType ctx tctx mod t
                resolveConstraint
                  ctx
                  tctx
                  mod
                  (TypeEq cArgType
                          t'
                          "Function arg type must match its annotation"
                          (argPos arg)
                  )
              _ -> return ()
            )
        _ -> throwk
          $ InternalError ("Expected function " ++ s_unpack name) Nothing
  DeclType t@(TypeDefinition { typeName = name })
    -> do
      -- TODO: params
      tctx     <- newTypeContext []
      subScope <- getSubScope (modScope mod) [name]
      forM_
        (typeStaticFields t)
        (\field -> do
          binding <- scopeGet subScope (varName field)
          case varType field of
            Just t -> do
              t' <- resolveType ctx tctx mod t
              resolveConstraint
                ctx
                tctx
                mod
                (TypeEq (bindingConcrete binding)
                        t'
                        "Static field type must match its type annotation"
                        -- FIXME: position
                        (NoPos)
                )
            _ -> return ()
        )
      -- modType <- resolveLocal (modScope mod) name

      -- case modType of
        -- TypeStruct _
      return ()
  _ -> return ()
