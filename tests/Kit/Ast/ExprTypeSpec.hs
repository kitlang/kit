module Kit.Ast.ExprTypeSpec where

import Test.Hspec
import Test.QuickCheck
import Kit.Ast

spec :: Spec
spec = do
  describe "exprMapReduce" $ do
    it "exprMapReduce traverses the full AST, post-order" $ do
      let
        ex =
          e
            (Block
              [e $ Continue, e $ Break, e $ Block [e $ This, e $ Self, e $ Block [e $ This]]]
            )
      exprMapReduce (exprDiscriminant . expr) (:) expr [] ex `shouldBe` map
        exprDiscriminant
        ([Continue, Break, This, Self, This, Block [], Block [], Block []] :: [ExprType () ()])
