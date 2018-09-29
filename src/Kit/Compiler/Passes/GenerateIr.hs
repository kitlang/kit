{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Kit.Compiler.Passes.GenerateIr where

import Control.Monad
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Ir
import Kit.Compiler.Module
import Kit.Compiler.TypedDecl
import Kit.Compiler.Utils
import Kit.HashTable
import Kit.Ir

{-
  Generates declarations in interediate representation for each typed module.
-}
generateIr
  :: CompileContext -> [(Module, [TypedDecl])] -> IO [(Module, [IrBundle])]
generateIr ctx modContent = forM modContent (generateModuleIr ctx)

generateModuleIr
  :: CompileContext -> (Module, [TypedDecl]) -> IO (Module, [IrBundle])
generateModuleIr ctx (mod, decls) = do
  debugLog ctx $ "generating IR for " ++ show mod
  decls  <- forM decls (generateDeclIr ctx mod)
  tuples <- h_toList (modTuples mod)
  return
    ( mod
    , [ IrBundle ([], n) [DeclTuple t]
      | (_, t@(BasicTypeTuple n parts)) <- tuples
      ]
      ++ (foldr (++) [] decls)
    )
