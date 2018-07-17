module Kit.Compiler.UnifySpec where

import Test.Hspec
import Test.QuickCheck
import Kit.Ast
import Kit.Compiler
import Kit.Str

testUnify a b c = do
  ctx <- newCompileContext
  tctx <- newTypeContext []
  mod <- newMod [] [] ""
  unification <- unify ctx tctx mod a b
  unification `shouldBe` c

spec :: Spec
spec = do
  describe "TypeSpec unification" $ do
    it "unifies type variables" $ do
      testUnify
        (TypeTypeVar $ TypeVar 1)
        (TypeTypeVar $ TypeVar 2)
        (TypeVarIs (TypeVar 1) (TypeTypeVar $ TypeVar 2))
      testUnify
        (TypeTypeVar $ TypeVar 1)
        (TypeStruct (["a", "b"], "mytype") [])
        (TypeVarIs (TypeVar 1) (TypeStruct (["a", "b"], "mytype") []))
      testUnify
        (TypeStruct (["a", "b"], "mytype") [])
        (TypeTypeVar $ TypeVar 1)
        (TypeVarIs (TypeVar 1) (TypeStruct (["a", "b"], "mytype") []))
  describe "Basic type unifiation" $ do
    it "unifies numeric types" $ do
      unifyBasic (BasicTypeInt 32) (BasicTypeInt 64)
        `shouldBe` TypeConstraintSatisfied
      unifyBasic (BasicTypeInt 64) (BasicTypeInt 32)
        `shouldBe` TypeConstraintSatisfied
      unifyBasic (BasicTypeInt 32) (BasicTypeUint 64)
        `shouldBe` TypeConstraintSatisfied
      unifyBasic (BasicTypeUint 32) (BasicTypeUint 64)
        `shouldBe` TypeConstraintSatisfied
      unifyBasic (BasicTypeUint 32) (BasicTypeFloat 64)
        `shouldBe` TypeConstraintSatisfied
      unifyBasic (BasicTypeInt 32) (BasicTypeFloat 64)
        `shouldBe` TypeConstraintSatisfied
      unifyBasic (BasicTypeFloat 32) (BasicTypeUint 64)
        `shouldBe` TypeConstraintNotSatisfied
