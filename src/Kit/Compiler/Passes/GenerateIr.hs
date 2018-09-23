{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Kit.Compiler.Passes.GenerateIr where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Generators
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Ir
import Kit.Parser
import Kit.Str

{-
  Generates declarations in interediate representation for each typed module.
-}
generateIr
  :: CompileContext -> [(Module, [TypedDecl])] -> IO [(Module, [DeclBundle])]
generateIr ctx modContent = forM modContent (generateModuleIr ctx)

generateModuleIr
  :: CompileContext -> (Module, [TypedDecl]) -> IO (Module, [DeclBundle])
generateModuleIr ctx (mod, decls) = do
  debugLog ctx $ "generating IR for " ++ show mod
  decls  <- forM decls (generateDeclIr ctx mod)
  tuples <- h_toList (modTuples mod)
  return
    ( mod
    , [ DeclBundle ([], n)
                   [DeclTuple t]
                   (foldr (++) [] $ map (typeDeps Def) parts)
      | (_, t@(BasicTypeTuple n parts)) <- tuples
      ]
      ++ (foldr (++) [] decls)
    )
