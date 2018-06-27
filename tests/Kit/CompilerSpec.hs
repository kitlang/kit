{-# OPTIONS_GHC -w #-}

module Kit.CompilerSpec where

  import Test.Hspec
  import Test.QuickCheck
  import Kit.Ast
  import Kit.Compiler
  import Kit.Error
  import Kit.HashTable
  import Kit.Parser
  import Kit.Str

  newVar s = VarBinding $ VarDefinition {
    var_name = Var s,
    var_doc = Nothing,
    var_meta = [],
    var_modifiers = [],
    var_type = Nothing,
    var_default = Nothing
  }

  expectFail :: CompileContext -> IO Bool
  expectFail ctx = do
    result <- tryCompile $ ctx
    case result of
      Left errs -> return True
      Right () -> return False

  spec :: Spec
  spec = do
    describe "tryCompile" $ do
      it "fails when main doesn't exist" $ do
        result <- expectFail compile_context {context_main_module = ["module", "that", "doesnt", "exist"]}
        result `shouldBe` True

      it "finds imports" $ do
        let exprs = case parseString "import a; import b.c; function main() {} import d;" of
                      ParseResult e -> e
                      Err _ -> []
        m <- newMod [] exprs
        map fst (mod_imports m) `shouldBe` [["a"], ["b", "c"], ["d"]]

    describe "Variable resolution" $ do
      it "resolves variables to scopes, falling back to modules" $ do
        m <- newMod ["abc"] []
        -- if we look for a binding in brokenMod, the test will fail
        let brokenMod = Module {}
        h_insert (mod_vars m) "a" (newVar "a1")
        h_insert (mod_vars m) "b" (newVar "b1")
        h_insert (mod_vars m) "c" (newVar "c1")
        s1 <- newScope
        bindToScope s1 "a" (newVar "a2")
        bindToScope s1 "b" (newVar "b2")
        s2 <- newScope
        bindToScope s2 "a" (newVar "a3")
        let scopes = [s2, s1]
        let ctx = compile_context
        fa <- findVar ctx scopes brokenMod "a"
        fb <- findVar ctx scopes brokenMod "b"
        fc <- findVar ctx scopes m "c"
        fd <- findVar ctx scopes m "d"
        fa `shouldBe` (Just $ newVar "a3")
        fb `shouldBe` (Just $ newVar "b2")
        fc `shouldBe` (Just $ newVar "c1")
        fd `shouldBe` Nothing
