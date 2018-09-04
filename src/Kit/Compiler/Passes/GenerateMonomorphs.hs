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
import Kit.Compiler.Generators.NameMangling
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers
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
    tctx       <- newTypeContext []
    params     <- forM params' (mapType $ follow ctx tctx)
    unresolved <- forM params $ typeUnresolved ctx tctx
    if or unresolved
      -- don't try to generate a monomorph if the params contain unresolved
      -- type variables; if we needed this, it'll blow up elsewhere
      then return Nothing
      else do
        existing <- h_lookup (ctxCompleteGenerics ctx) (tp, params)
        case existing of
          Just x -> return Nothing
          _      -> do
            h_insert (ctxCompleteGenerics ctx) (tp, params) ()
            definitionMod <- getMod ctx modPath
            binding       <- scopeGet (modScope definitionMod) name
            case binding of
              FunctionBinding def -> do
                x <- typeFunctionMonomorph ctx definitionMod def params
                return $ case x of
                  Just (DeclFunction x) -> Just
                    ( definitionMod
                    , [ DeclFunction $ x
                          { functionName = monomorphName (functionName def)
                                                         params
                          }
                      ]
                    )
                  _ -> Nothing

              TypeBinding def -> do
                x <- typeTypeMonomorph ctx definitionMod def params
                return $ case x of
                  Just (DeclType x) -> Just
                    ( definitionMod
                    , [ DeclType
                          $ x { typeName = monomorphName (typeName def) params }
                      ]
                    )
                  _ -> Nothing

              TraitBinding def -> do
                x <- typeTraitMonomorph ctx definitionMod def params
                return $ case x of
                  Just (DeclTrait x) -> Just
                    ( definitionMod
                    , [ DeclTrait $ x
                          { traitName = monomorphName (traitName def) params
                          }
                      ]
                    )
                  _ -> Nothing
              _ -> return Nothing

  return $ catMaybes decls
