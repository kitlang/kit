module Kit.Compiler.Passes.IncludeCModulesSpec where

  import Control.Monad
  import Test.Hspec
  import Test.QuickCheck
  import Kit.Ast
  import Kit.CodeGen.C
  import Kit.Compiler.Passes
  import Kit.Ir

  spec :: Spec
  spec = do
    describe "Parses C type declarations" $ do
      forM_ [
          BasicTypeVoid, BasicTypeBool,
          BasicTypeInt 8, BasicTypeInt 16, BasicTypeInt 32, BasicTypeInt 64,
          BasicTypeUint 8, BasicTypeUint 16, BasicTypeUint 32, BasicTypeUint 64
        ] (\t -> it ("Parses C specifiers into " ++ show t) $ parseDeclSpec (ctype t) `shouldBe` Just (TypeBasicType t))

      it "Resolves specifiers for structs into struct types" $ do
        parseDeclSpec (ctype (BasicTypeStruct ("mystruct", [("a", BasicTypeInt 8), ("b", BasicTypeUint 16)]))) `shouldBe` Just (TypeStruct [] "mystruct")
        parseDeclSpec (ctype (BasicTypeComplexEnum "myenum" [])) `shouldBe` Just (TypeStruct [] "myenum")

      it "Resolves specifiers for basic enums into enum types" $ do
        parseDeclSpec (ctype (BasicTypeSimpleEnum "myenum" [])) `shouldBe` Just (TypeEnum [] "myenum")
