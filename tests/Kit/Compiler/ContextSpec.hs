{-# OPTIONS_GHC -w #-}

module Kit.Compiler.ContextSpec where

import Test.Hspec
import Test.QuickCheck
import Kit.Ast
import Kit.Compiler
import Kit.Compiler.Passes
import Kit.Error
import Kit.HashTable
import Kit.Parser
import Kit.Str

newVar s = VarBinding
  (newVarDefinition { varName = ([], s), varType = TypeBasicType BasicTypeVoid }
  )

spec :: Spec
spec = do
  describe "CompileContext" $ do
    it "generates headers/code in the correct directories" $ do
      ctx <- newCompileContext
      includePath ctx ([]       , "apple") `shouldBe` "build/include/kit_apple.h"
      includePath ctx (["apple"], "banana")
        `shouldBe` "build/include/apple/kit_banana.h"
      libPath ctx ([]       , "apple") `shouldBe` "build/lib/kit_apple.c"
      libPath ctx (["apple"], "banana") `shouldBe` "build/lib/apple/kit_banana.c"

  describe "Variable resolution" $ do
    it "resolves variables to scopes, falling back to modules" $ do
      ctx <- newCompileContext
      m   <- newMod ["abc"] ""
      h_insert (ctxModules ctx) (modPath m) m
      -- if we look for a binding in brokenMod, the test will fail
      let brokenMod = undefined
      addBinding ctx (modPath m, "a") $ newVar "a1"
      addBinding ctx (modPath m, "b") $ newVar "b1"
      addBinding ctx (modPath m, "c") $ newVar "c1"
      s1 <- newScope
      bindToScope s1 "a" $ newVar "a2"
      bindToScope s1 "b" $ newVar "b2"
      s2 <- newScope
      bindToScope s2 "a" $ newVar "a3"
      let scopes = [s2, s1]
      fa  <- resolveVar ctx scopes brokenMod "a"
      fb  <- resolveVar ctx scopes brokenMod "b"
      fc  <- resolveVar ctx scopes m "c"
      fd  <- resolveVar ctx scopes m "d"
      fa `shouldBe` (Just $ newVar "a3")
      fb `shouldBe` (Just $ newVar "b2")
      fc `shouldBe` (Just $ newVar "c1")
      fd `shouldBe` Nothing
