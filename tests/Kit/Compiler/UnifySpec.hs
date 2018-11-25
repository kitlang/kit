module Kit.Compiler.UnifySpec where

import Test.Hspec
import Test.QuickCheck
import Kit.Ast
import Kit.Compiler
import Kit.Parser
import Kit.Str

testUnify ctx a b c = do
  tctx        <- newTypeContext []
  mod         <- newMod [] ""
  unification <- unify ctx tctx a b
  unification `shouldBe` c

spec :: Spec
spec = do
  describe "MethodTarget unification" $ do
    it "unifies method targets" $ do
      ctx <- newCompileContext
      let t = TypeBasicType BasicTypeCInt
      -- testUnify ctx t                (MethodTarget t) Nothing
      testUnify ctx (MethodTarget t) t                Nothing
      testUnify ctx (MethodTarget t) (MethodTarget t) (Just [])
      testUnify ctx
                (MethodTarget (TypePtr voidType))
                (MethodTarget (TypePtr t))
                (Just [])
  describe "TypeSpec unification" $ do
    it "unifies type variables" $ do
      ctx <- newCompileContext
      a   <- makeTypeVar ctx (sp "" 1 1 1 1)
      b   <- makeTypeVar ctx (sp "" 1 1 1 1)
      testUnify ctx a b (Just [TypeVarIs 1 b])
      testUnify ctx
                a
                (TypeInstance (["a", "b"], "mytype") [])
                (Just [TypeVarIs 1 (TypeInstance (["a", "b"], "mytype") [])])
      testUnify ctx
                (TypeInstance (["a", "b"], "mytype") [])
                a
                (Just [TypeVarIs 1 (TypeInstance (["a", "b"], "mytype") [])])
  describe "Basic type unifiation" $ do
    it "unifies numeric types" $ do
      unifyBasic (BasicTypeInt 32)   (BasicTypeInt 64) `shouldBe` Just []
      unifyBasic (BasicTypeInt 64)   (BasicTypeInt 32) `shouldBe` Just []
      unifyBasic (BasicTypeInt 32)   (BasicTypeUint 64) `shouldBe` Just []
      unifyBasic (BasicTypeUint 32)  (BasicTypeUint 64) `shouldBe` Just []
      unifyBasic (BasicTypeUint 32)  (BasicTypeFloat 64) `shouldBe` Nothing
      unifyBasic (BasicTypeInt 32)   (BasicTypeFloat 64) `shouldBe` Nothing
      unifyBasic (BasicTypeFloat 32) (BasicTypeUint 64) `shouldBe` Just []
      unifyBasic (BasicTypeFloat 32) (BasicTypeInt 64) `shouldBe` Just []
