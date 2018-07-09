module Kit.Compiler.UnifySpec where

  import Test.Hspec
  import Test.QuickCheck
  import Kit.Ast
  import Kit.Compiler
  import Kit.Str

  spec :: Spec
  spec = do
    describe "TypeSpec unification" $ do
      it "unifies type variables" $ do
        unify (TypeVar 1) (TypeVar 2) `shouldBe` TypeVarIs 2 (TypeVar 1)
        unify (TypeVar 1) (TypeStruct (["a", "b"], "mytype") []) `shouldBe` TypeVarIs 1 (TypeStruct (["a", "b"], "mytype") [])
        unify (TypeStruct (["a", "b"], "mytype") []) (TypeVar 1) `shouldBe` TypeVarIs 1 (TypeStruct (["a", "b"], "mytype") [])
    describe "Basic type unifiation" $ do
      it "unifies numeric types" $ do
        unifyBasic (BasicTypeInt 32) (BasicTypeInt 64) `shouldBe` TypeConstraintSatisfied
        unifyBasic (BasicTypeInt 64) (BasicTypeInt 32) `shouldBe` TypeConstraintSatisfied
        unifyBasic (BasicTypeInt 32) (BasicTypeUint 64) `shouldBe` TypeConstraintSatisfied
        unifyBasic (BasicTypeUint 32) (BasicTypeUint 64) `shouldBe` TypeConstraintSatisfied
        unifyBasic (BasicTypeUint 32) (BasicTypeFloat 64) `shouldBe` TypeConstraintSatisfied
        unifyBasic (BasicTypeInt 32) (BasicTypeFloat 64) `shouldBe` TypeConstraintSatisfied
        unifyBasic (BasicTypeFloat 32) (BasicTypeUint 64) `shouldBe` TypeConstraintNotSatisfied
