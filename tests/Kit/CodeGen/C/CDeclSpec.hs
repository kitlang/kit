module Kit.CodeGen.C.CDeclSpec where

  import Test.Hspec
  import Test.QuickCheck
  import Text.PrettyPrint
  import Language.C
  import Data.List
  import Kit.Ir
  import Kit.Str
  import Kit.CodeGen.C

  showstruct x = intercalate "\n" [renderStyle (Style {mode = LeftMode}) $ pretty $ xi | xi <- cdecl x]
  showdisc name variant_names = renderStyle (Style {mode = LeftMode}) $ pretty $ enum_discriminant name variant_names

  spec :: Spec
  spec = do
    describe "Transpiles structures into typedefs" $ do
      it "Transpiles structs" $ do
        showstruct (TypeStruct ("MyStruct", [("a", TypeInt 8), ("b", TypeUint 8)])) `shouldBe` "typedef struct {\nsigned char a; unsigned char b;\n} MyStruct"

    describe "Transpiles simple enums" $ do
      it "Transpiles enum discriminants" $ do
        showdisc "Apple" ["GrannySmith", "GoldenDelicious", "RedDelicious"] `shouldBe` "enum Apple {\nGrannySmith, GoldenDelicious, RedDelicious\n}"

      it "Transpiles simple enums" $ do
        showstruct (TypeSimpleEnum "MyEnum" ["Option1", "AnotherOption", "TheThirdOption"]) `shouldBe` "enum MyEnum {\nOption1, AnotherOption, TheThirdOption\n}"

      it "Transpiles complex enums" $ do
        shouldBe
          (showstruct (TypeComplexEnum "MyEnum" [("Variant1", []), ("Variant2", [("field1", TypeInt 8), ("field2", TypeUint 8)])]))
          "enum MyEnum_Discriminant {\nVariant1, Variant2\n}\n\
          \typedef struct {\nMyEnum_Discriminant __discriminant;\nunion {\nMyEnum_Variant_Variant2 variant_Variant2;\n} __variant;\n} MyEnum\n\
          \typedef struct {\nsigned char field1; unsigned char field2;\n} MyEnum_Variant_Variant2"
