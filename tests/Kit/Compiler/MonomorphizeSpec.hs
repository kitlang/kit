module Kit.Compiler.MonomorphizeSpec where

import Test.Hspec
import Test.QuickCheck
import Kit.Ast
import Kit.Compiler
import Kit.Str

testParams =
  [(makeTypeSpec "A", makeTypeSpec "A1"), (makeTypeSpec "B", makeTypeSpec "B1")]

spec :: Spec
spec = do
  describe "Monomorphizes expressions" $ do
    it "Substitutes monomorph type parameters in casts" $ do
      shouldBe
        (monomorphizeExpr testParams (e (Cast (e This) (makeTypeSpec "A"))))
        (e (Cast (e This) (makeTypeSpec "A1")))
      shouldBe
        (monomorphizeExpr testParams (e (Cast (e This) (makeTypeSpec "B"))))
        (e (Cast (e This) (makeTypeSpec "B1")))

    it "Substitutes monomorph type parameters in annotations" $ do
      shouldBe
        (monomorphizeExpr testParams
                          (e (TypeAnnotation (e This) (makeTypeSpec "A")))
        )
        (e (TypeAnnotation (e This) (makeTypeSpec "A1")))
      shouldBe
        (monomorphizeExpr testParams
                          (e (TypeAnnotation (e This) (makeTypeSpec "B")))
        )
        (e (TypeAnnotation (e This) (makeTypeSpec "B1")))

    it "Substitutes monomorph type parameters in blocks" $ do
      shouldBe
        (monomorphizeExpr testParams $ e
          (Block
            [ e (Cast (e This) (makeTypeSpec "A"))
            , e (TypeAnnotation (e This) (makeTypeSpec "B"))
            ]
          )
        )
        (e
          (Block
            [ e (Cast (e This) (makeTypeSpec "A1"))
            , e (TypeAnnotation (e This) (makeTypeSpec "B1"))
            ]
          )
        )

  describe "Monomorphizes types" $ do
    it "Monomorphizes structs" $ do
      True `shouldBe` True
