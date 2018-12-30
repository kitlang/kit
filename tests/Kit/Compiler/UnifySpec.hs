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
                (MethodTarget (TypePtr TypeVoid))
                (MethodTarget (TypePtr t))
                (Just [])
  describe "TypeSpec unification" $ do
    it "unifies type variables" $ do
      ctx <- newCompileContext
      a   <- makeTypeVar ctx (sp "" 1 1 1 1)
      b   <- makeTypeVar ctx (sp "" 1 1 1 1)
      testUnify ctx a b (Just [TypeVarIs 0 b])
      testUnify ctx
                a
                (TypeInstance (["a", "b"], "mytype") [])
                (Just [TypeVarIs 0 (TypeInstance (["a", "b"], "mytype") [])])
      testUnify ctx
                (TypeInstance (["a", "b"], "mytype") [])
                a
                (Just [TypeVarIs 0 (TypeInstance (["a", "b"], "mytype") [])])
  -- describe "Numeric unifiation" $ do
  --   it "unifies numeric types" $ do
  --     ctx <- newCompileContext
  --     let t = testUnify ctx
  --     t (TypeInt 32)   (TypeInt 64) $ Just []
  --     t (TypeInt 64)   (TypeInt 32) $ Just []
  --     t (TypeInt 32)   (TypeUint 64) $ Just []
  --     t (TypeUint 32)  (TypeUint 64) $ Just []
  --     t (TypeUint 32)  (TypeFloat 64) $ Nothing
  --     t (TypeInt 32)   (TypeFloat 64) $ Nothing
  --     t (TypeFloat 32) (TypeUint 64) $ Just []
  --     t (TypeFloat 32) (TypeInt 64) $ Just []
