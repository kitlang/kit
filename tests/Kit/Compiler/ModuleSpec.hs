module Kit.Compiler.ModuleSpec where

  import Test.Hspec
  import Test.QuickCheck
  import Kit.Ast
  import Kit.Compiler
  import Kit.Compiler.Passes
  import Kit.Error
  import Kit.HashTable
  import Kit.Parser
  import Kit.Str

  newVar s = VarBinding $ TypeBasicType BasicTypeVoid

  expectFail :: CompileContext -> IO Bool
  expectFail ctx = do
    result <- tryCompile $ ctx
    case result of
      Left errs -> return True
      Right () -> return False

  spec :: Spec
  spec = do
    describe "tryCompile" $ do
      it "finds imports" $ do
        let exprs = case parseString "import a; import b.c; function main() {} import d;" of
                      ParseResult e -> e
                      Err _ -> []
        m <- newMod [] exprs
        map fst (mod_imports m) `shouldBe` [["a"], ["b", "c"], ["d"]]
