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

newVar s = newBinding VarBinding (TypeBasicType BasicTypeVoid) Nothing null_span

spec :: Spec
spec = do
  describe "CompileContext" $ do
    it "generates headers/code in the correct directories" $ do
      ctx <- newCompileContext
      includePath ctx ["apple"] `shouldBe` "build/include/apple.h"
      includePath ctx ["apple", "banana"]
        `shouldBe` "build/include/apple/banana.h"
      libPath ctx ["apple"] `shouldBe` "build/lib/apple.c"
      libPath ctx ["apple", "banana"] `shouldBe` "build/lib/apple/banana.c"

  describe "Variable resolution" $ do
    it "resolves variables to scopes, falling back to modules" $ do
      m <- newMod ["abc"] ""
      -- if we look for a binding in brokenMod, the test will fail
      let brokenMod = undefined--Module {}
      bindToScope (modScope m) "a" (newVar "a1")
      bindToScope (modScope m) "b" (newVar "b1")
      bindToScope (modScope m) "c" (newVar "c1")
      s1 <- newScope
      bindToScope s1 "a" (newVar "a2")
      bindToScope s1 "b" (newVar "b2")
      s2 <- newScope
      bindToScope s2 "a" (newVar "a3")
      let scopes = [s2, s1]
      ctx <- newCompileContext
      fa  <- resolveVar ctx scopes brokenMod "a"
      fb  <- resolveVar ctx scopes brokenMod "b"
      fc  <- resolveVar ctx scopes m "c"
      fd  <- resolveVar ctx scopes m "d"
      fa `shouldBe` (Just $ newVar "a3")
      fb `shouldBe` (Just $ newVar "b2")
      fc `shouldBe` (Just $ newVar "c1")
      fd `shouldBe` Nothing
