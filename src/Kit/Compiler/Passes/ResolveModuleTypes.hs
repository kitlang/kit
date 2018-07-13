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
import Kit.Compiler.TypeUsage
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
  main <- resolveLocal (mod_vars mod) "main"
  case main of
    Just (FunctionBinding _ _ _) -> return ()
    _                            -> throw $ Errs
      [ err ValidationError
        $ (show mod)
        ++ " doesn't have a function called 'main'; main module requires a main function"
      ]

findTopLevels :: CompileContext -> Module -> IO ()
findTopLevels ctx mod = do
  contents <- readIORef $ mod_contents mod
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
    TypeDeclaration t -> do
      debugLog ctx
        $  "found type "
        ++ s_unpack (type_name t)
        ++ " in "
        ++ (show mod)
      usage <- newTypeUsage t
      bindToScope (mod_type_definitions mod) (type_name t) usage
      ct <- typeDefinitionToConcreteType ctx tctx mod t
      bindToScope (mod_types mod) (type_name t) ct
      case t of
        TypeDefinition { type_name = type_name, type_type = Enum { enum_variants = variants } }
          -> do
            forM_
              (variants)
              (\variant -> do
                args <- mapM
                  (\arg -> do
                    -- FIXME: this is the statement's position, not the arg's position
                    t <- resolveMaybeType ctx tctx mod (stmtPos s) (arg_type arg)
                    return (arg_name arg, t)
                  )
                  (variant_args variant)
                let constructor =
                      EnumConstructor ((mod_path mod), type_name) args
                bindToScope (mod_vars mod) (variant_name variant) constructor
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
      bindToScope (mod_types mod) a b'
    ModuleVarDeclaration v -> do
      debugLog ctx
        $  "found variable "
        ++ s_unpack (var_name v)
        ++ " in "
        ++ (show mod)
      varType <- resolveMaybeType ctx tctx mod (stmtPos s) (var_type v)
      bindToScope (mod_vars mod) (var_name v) (VarBinding (varType))
    FunctionDeclaration f -> do
      debugLog ctx
        $  "found function "
        ++ s_unpack (function_name f)
        ++ " in "
        ++ (show mod)
      functionType <- resolveMaybeType ctx tctx mod (stmtPos s) (function_type f)
      args         <- mapM
        (\arg -> do
          t <- resolveMaybeType ctx tctx mod (stmtPos s) (arg_type arg)
          return (arg_name arg, t)
        )
        (function_args f)
      bindToScope (mod_vars mod)
                  (function_name f)
                  (FunctionBinding (functionType) args (function_varargs f))
      bindToScope (mod_functions mod) (function_name f) f
    _ -> return ()
