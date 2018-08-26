{-# OPTIONS_GHC -w #-}

module Kit.CodeGen.C.CDeclSpec where

import Test.Hspec
import Test.QuickCheck
import Text.PrettyPrint
import Language.C
import Data.List
import Kit.Ast
import Kit.Ir
import Kit.Str
import Kit.CodeGen.C

showstruct x = intercalate
  "\n"
  [ renderStyle (Style {mode = LeftMode}) $ pretty $ xi | xi <- cdecl x ]
showdisc name variantNames =
  renderStyle (Style {mode = LeftMode}) $ pretty $ enumDiscriminant
    (Just name)
    variantNames

spec :: Spec
spec = do
  describe "Transpiles structures into typedefs" $ do
    it "Transpiles structs" $ do
      showstruct
          (BasicTypeStruct (Just "MyStruct")
                           [("a", BasicTypeInt 8), ("b", BasicTypeUint 8)]
          )
        `shouldBe` "struct MyStruct {\nint8_t a; uint8_t b;\n}"

  describe "Transpiles simple enums" $ do
    it "Transpiles enum discriminants" $ do
      showdisc "Apple" ["GrannySmith", "GoldenDelicious", "RedDelicious"]
        `shouldBe` "enum Apple {\nGrannySmith, GoldenDelicious, RedDelicious\n}"

    it "Transpiles simple enums" $ do
      showstruct
          (BasicTypeSimpleEnum (Just "MyEnum")
                               ["Option1", "AnotherOption", "TheThirdOption"]
          )
        `shouldBe` "enum MyEnum {\nOption1, AnotherOption, TheThirdOption\n}"

    it "Transpiles complex enums" $ do
      shouldBe
        (showstruct
          (BasicTypeComplexEnum
            "MyEnum"
            [ ("Variant1", [])
            , ( "Variant2"
              , [("field1", BasicTypeCChar), ("field2", BasicTypeUint 8)]
              )
            ]
          )
        )
        "struct MyEnum_Variant_Variant2 {\nchar field1; uint8_t field2;\n}\n\
        \enum MyEnum_Discriminant {\nVariant1, Variant2\n}\n\
        \struct MyEnum {\nenum MyEnum_Discriminant __dsc;\nunion {\nstruct MyEnum_Variant_Variant2 variant_Variant2;\n} __var;\n}"
