module Kit.Ast.ExprSpec where

import Test.Hspec
import Kit.Ast

exprSubFunction e = case (expr e) of
  Lvalue (Var "replace") -> e { expr = Lvalue (Var "with") }
  _                      -> e

spec :: Spec
spec = do
  describe "exprMap" $ do
    it "maps to expression children recursively" $ do
      shouldBe
        (exprMap
          exprSubFunction
          (e
            (Binop
              Add
              (e $ Lvalue $ Var "replace")
              (e $ Binop Sub
                         (e $ Lvalue $ Var "somethingelse")
                         (e $ Lvalue $ Var "replace")
              )
            )
          )
        )
        (e
          (Binop
            Add
            (e $ Lvalue $ Var "with")
            (e $ Binop Sub
                       (e $ Lvalue $ Var "somethingelse")
                       (e $ Lvalue $ Var "with")
            )
          )
        )

    it "maps blocks" $ do
      shouldBe
        (exprMap
          exprSubFunction
          (e
            (Block
              [ e $ Lvalue $ Var "replace"
              , e $ Lvalue $ Var "something"
              , e $ Lvalue $ Var "replace"
              ]
            )
          )
        )
        (e
          (Block
            [ e $ Lvalue $ Var "with"
            , e $ Lvalue $ Var "something"
            , e $ Lvalue $ Var "with"
            ]
          )
        )
