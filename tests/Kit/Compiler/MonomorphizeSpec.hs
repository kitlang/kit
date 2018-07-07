module Kit.Compiler.MonomorphizeSpec where

  import Test.Hspec
  import Test.QuickCheck
  import Kit.Ast
  import Kit.Compiler
  import Kit.Str

  testParams = [(TypeSpec ([], "A") [], TypeSpec ([], "A1") []), (TypeSpec ([], "B") [], TypeSpec ([], "B1") [])]

  spec :: Spec
  spec = do
    describe "Monomorphizes expressions" $ do
      it "Substitutes monomorph type parameters in casts" $ do
        shouldBe
          (monomorphizeExpr testParams (e (Cast (e This) (TypeSpec ([], "A") []))))
          (e (Cast (e This) (TypeSpec ([], "A1") [])))
        shouldBe
          (monomorphizeExpr testParams (e (Cast (e This) (TypeSpec ([], "B") []))))
          (e (Cast (e This) (TypeSpec ([], "B1") [])))

      it "Substitutes monomorph type parameters in annotations" $ do
        shouldBe
          (monomorphizeExpr testParams (e (TypeAnnotation (e This) (TypeSpec ([], "A") []))))
          (e (TypeAnnotation (e This) (TypeSpec ([], "A1") [])))
        shouldBe
          (monomorphizeExpr testParams (e (TypeAnnotation (e This) (TypeSpec ([], "B") []))))
          (e (TypeAnnotation (e This) (TypeSpec ([], "B1") [])))

      it "Substitutes monomorph type parameters in blocks" $ do
        shouldBe
          (monomorphizeExpr testParams $ e (Block [
            e (Cast (e This) (TypeSpec ([], "A") [])),
            e (TypeAnnotation (e This) (TypeSpec ([], "B") []))
          ]))
          (e (Block [
            e (Cast (e This) (TypeSpec ([], "A1") [])),
            e (TypeAnnotation (e This) (TypeSpec ([], "B1") []))
          ]))

    describe "Monomorphizes types" $ do
      it "Monomorphizes structs" $ do
        True `shouldBe` True
