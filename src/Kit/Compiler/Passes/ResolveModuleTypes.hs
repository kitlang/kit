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

resolveModuleTypes :: CompileContext -> IO ()
resolveModuleTypes ctx = do
  mods <- h_toList $ ctxModules ctx
  forM_ (map snd mods) (findTopLevels ctx)
  validateMain ctx
  return ()

validateMain :: CompileContext -> IO ()
validateMain ctx = do
  mod  <- getMod ctx (ctxMainModule ctx)
  main <- resolveLocal (modVars mod) "main"
  case main of
    Just (Binding { bindingType = FunctionBinding _ _ _ }) -> return ()
    _ -> throw $ KitError $ BasicError
      (show mod
      ++ " doesn't have a function called 'main'; main module requires a main function"
      )
      (Nothing)

findTopLevels :: CompileContext -> Module -> IO ()
findTopLevels ctx mod = do
  contents <- readIORef $ modContents mod
  forM_ contents (_checkForTopLevel ctx mod)
  return ()

{-
  Parse module toplevel statements, which can be type, function, or variable
  declarations, and try to resolve their types recursively. Fail here if any
  unknown types are referenced.
-}
_checkForTopLevel :: CompileContext -> Module -> Statement -> IO ()
_checkForTopLevel ctx mod s = do
  tctx <- newTypeContext []
  case stmt s of
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
        TypeDefinition { typeName = typeName, typeType = Enum { enum_variants = variants } }
          -> do
            forM_
              (variants)
              (\variant -> do
                args <- mapM
                  (\arg -> do
                    -- FIXME: this is the statement's position, not the arg's position
                    t <- resolveMaybeType ctx tctx mod (stmtPos s) (argType arg)
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
    _ -> return ()
