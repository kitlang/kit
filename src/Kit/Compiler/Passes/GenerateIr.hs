{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Kit.Compiler.Passes.GenerateIr where

import Control.Monad
import Data.IORef
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Ir
import Kit.Compiler.Module
import Kit.Compiler.TypedStmt
import Kit.Compiler.Utils
import Kit.HashTable
import Kit.Ir

{-
  Generates declarations in interediate representation for each typed module.
-}
generateIr
  :: CompileContext -> [(Module, [TypedStmt])] -> IO [(Module, [IrBundle])]
generateIr ctx modContent = do
  modContent     <- forM modContent (generateModuleIr ctx)
  concreteTuples <- readIORef $ ctxTuples ctx
  basicTuples    <- forM concreteTuples $ \(mod, t) -> do
    mod <- getMod ctx mod
    t   <- findUnderlyingType ctx mod Nothing t
    return t
  mod <- getMod ctx (ctxMainModule ctx)
  return
    $ ( mod
      , [ IrBundle ([], n) [ps NoPos $ TupleDeclaration t]
        | t@(BasicTypeTuple n _) <- nub basicTuples
        ]
      )
    : modContent

generateModuleIr
  :: CompileContext -> (Module, [TypedStmt]) -> IO (Module, [IrBundle])
generateModuleIr ctx (mod, decls) = do
  debugLog ctx $ "generating IR for " ++ show mod
  decls <- forM decls (generateDeclIr ctx mod)
  return $ (mod, (foldr (++) [] decls))
