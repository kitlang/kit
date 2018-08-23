module Kit.Compiler.Passes.GenerateMonomorphs where

import Control.Exception
import Control.Monad
import Data.Function
import Data.IORef
import Data.List
import Data.Maybe
import System.FilePath
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.DumpAst
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Parser
import Kit.Str

generateMonomorphs :: CompileContext -> IO [(Module, [TypedDecl])]
generateMonomorphs ctx = do
  pendingGenerics <- readIORef (ctxPendingGenerics ctx)
  writeIORef (ctxPendingGenerics ctx) []
  decls <- forM (reverse pendingGenerics) $ \(tp@(modPath, name), params') -> do
    tctx     <- newTypeContext []
    params   <- forM params' (mapType $ knownType ctx tctx)
    existing <- h_lookup (ctxCompleteGenerics ctx) (tp, params)
    case existing of
      Just x -> return Nothing
      _      -> do
        h_insert (ctxCompleteGenerics ctx) (tp, params) ()
        definitionMod <- getMod ctx modPath
        binding       <- scopeGet (modScope definitionMod) name
        case bindingType binding of
          FunctionBinding def -> do
            x <- typeFunctionMonomorph
              ctx
              definitionMod
              (def { functionName = monomorphName (functionName def) params })
              params
            return $ case x of
              (Just x, _) -> Just (definitionMod, [x])
              _           -> Nothing
          _ -> return Nothing

  return $ catMaybes decls
