module Kit.CompilerSpec where

  import Test.Hspec
  import Test.QuickCheck
  import Kit.Compiler
  import Kit.Error
  import Kit.Parser
  import Kit.Str

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
        mod_imports (new_mod [] exprs) `shouldBe` [["a"], ["b", "c"], ["d"]]
