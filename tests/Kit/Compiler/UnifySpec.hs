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
        unify (TypeVar 1) (TypeVar 2) `shouldBe` (True, [(TypeVar 1, TypeVar 2)])
        unify (TypeVar 1) (TypeSpec (["a", "b"], "mytype") []) `shouldBe` (True, [(TypeVar 1, (TypeSpec (["a", "b"], "mytype") []) )])
        unify (TypeSpec (["a", "b"], "mytype") []) (TypeVar 1) `shouldBe` (True, [(TypeVar 1, (TypeSpec (["a", "b"], "mytype") []) )])
    describe "Basic type unifiation" $ do
      it "unifies numeric types" $ do
        unifyBasic (BasicTypeInt 32) (BasicTypeInt 64) `shouldBe` (True, [])
        unifyBasic (BasicTypeInt 64) (BasicTypeInt 32) `shouldBe` (True, [])
        unifyBasic (BasicTypeInt 32) (BasicTypeUint 64) `shouldBe` (True, [])
        unifyBasic (BasicTypeUint 32) (BasicTypeUint 64) `shouldBe` (True, [])
        unifyBasic (BasicTypeUint 32) (BasicTypeFloat 64) `shouldBe` (True, [])
        unifyBasic (BasicTypeInt 32) (BasicTypeFloat 64) `shouldBe` (True, [])
        unifyBasic (BasicTypeFloat 32) (BasicTypeUint 64) `shouldBe` (False, [])
