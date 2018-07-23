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
    case file pos2 of
      Just fp -> displayFileSnippet (s_unpack fp) pos2
      _ -> return ()
    ePutStrLn "\nTraits cannot have overlapping specializations."
  errPos (DuplicateSpecializationError _ _ pos _) = Just pos

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

{-
  Parse module toplevel statements, which can be type, function, or variable
  declarations, and try to resolve their types recursively. Fail here if any
  unknown types are referenced.
-}
_checkForTopLevel :: CompileContext -> Module -> Statement -> IO ()
_checkForTopLevel ctx mod s = do
  tctx <- newTypeContext []
  return ()
  {-case stmt s of
    {-TraitDeclaration t -> do
      h_insert (modTraits mod) (traitName t) t
    Implement t -> do
      -- TODO
      implTrait <-
      h_insert (modImpls mod) (traitName t) t-}
    Specialize trait inst -> do
      -- TODO
      return ()
    TypeDeclaration t -> do
      debugLog ctx
        $  "found type "
        ++ s_unpack (typeName t)
        ++ " in "
        ++ (show mod)
      --bindToScope (mod_type_definitions mod) (typeName t) usage
      ct <- typeDefinitionToConcreteType ctx tctx mod t
      bindToScope (modTypes mod)
                  (typeName t)
                  (newTypeBinding (BindingType t) ct)
      case t of
        TypeDefinition { typeName = typeName, typeType = Enum { enumVariants = variants } }
          -> do
            forM_
              (variants)
              (\variant -> do
                args <- mapM
                  (\arg -> do
                    t <- resolveMaybeType ctx tctx mod (argPos arg) (argType arg)
                    return (argName arg, t)
                  )
                  (variantArgs variant)
                let constructor =
                      EnumConstructor ((modPath mod), typeName) args
                bindToScope (modVars mod)
                            (variantName variant)
                            (newBinding constructor Nothing)
                return ()
              )
        _ -> do
          return ()
    Typedef a b -> do
      debugLog ctx
        $  "found typedef "
        ++ s_unpack a
        ++ " -> "
        ++ (show b)
        ++ " in "
        ++ (show mod)
      b' <- resolveType ctx tctx mod b
      bindToScope (modTypes mod) a (newTypeBinding (BindingTypedef) (b'))
    ModuleVarDeclaration v -> do
      debugLog ctx
        $  "found variable "
        ++ s_unpack (varName v)
        ++ " in "
        ++ (show mod)
      varType <- resolveMaybeType ctx tctx mod (stmtPos s) (varType v)
      bindToScope (modVars mod)
                  (varName v)
                  (newBinding (VarBinding (varType)) (Just $ modPath mod))
    FunctionDeclaration f -> do
      debugLog ctx
        $  "found function "
        ++ s_unpack (functionName f)
        ++ " in "
        ++ (show mod)
      functionType <- resolveMaybeType ctx tctx mod (stmtPos s) (functionType f)
      args         <- mapM
        (\arg -> do
          t <- resolveMaybeType ctx tctx mod (stmtPos s) (argType arg)
          return (argName arg, t)
        )
        (functionArgs f)
      bindToScope
        (modVars mod)
        (functionName f)
        (newBinding (FunctionBinding (functionType) args (functionVarargs f))
                    (Just $ modPath mod)
        )
      bindToScope (modFunctions mod)
                  (functionName f)
                  (f { functionNameMangling = Just $ modPath mod })
    _ -> return ()-}
