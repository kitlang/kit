{-# OPTIONS_GHC -w #-}

module Kit.CompilerSpec where

import Test.Hspec
import Test.QuickCheck
import Kit.Ast
import Kit.Compiler
import Kit.Compiler.Passes
import Kit.Error
import Kit.HashTable
import Kit.Parser
import Kit.Str

expectFail :: CompileContext -> IO Bool
expectFail ctx = do
  result <- tryCompile $ ctx
  case result of
    Left  errs -> return True
    Right ()   -> return False

spec :: Spec
spec = do
  describe "tryCompile" $ do
    it "fails when main doesn't exist" $ do
      ctx    <- newCompileContext
      result <- expectFail ctx
        { ctxMainModule = ["module", "that", "doesnt", "exist"]
        }
      result `shouldBe` True
