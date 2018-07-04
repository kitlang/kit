module Kit.Compiler.ScopeSpec where

  import Test.Hspec
  import Test.QuickCheck
  import Kit.Compiler
  import Kit.Str

  spec :: Spec
  spec = do
    describe "Scope resolution" $ do
      it "resolves values from scopes" $ do
        scope1 <- newScope
        bindToScope scope1 "a" 1
        scope2 <- newScope
        bindToScope scope2 "a" 2
        bindToScope scope2 "b" 3
        scope3 <- newScope
        bindToScope scope3 "a" 4
        bindToScope scope3 "b" 5
        bindToScope scope3 "c" 6
        let scopes = [scope1, scope2, scope3]

        a <- resolveBinding scopes "a"
        b <- resolveBinding scopes "b"
        c <- resolveBinding scopes "c"
        d <- resolveBinding scopes "d"
        localA3 <- resolveLocal scope3 "a"
        a `shouldBe` Just 1
        b `shouldBe` Just 3
        c `shouldBe` Just 6
        d `shouldBe` Nothing
        localA3 `shouldBe` Just 4
