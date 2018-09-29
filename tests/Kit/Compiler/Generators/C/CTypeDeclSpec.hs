{-# OPTIONS_GHC -w #-}

module Kit.Compiler.Generators.C.CTypeDeclSpec where

import Test.Hspec
import Test.QuickCheck
import Text.PrettyPrint
import Language.C
import Data.List
import Kit.Ast
import Kit.Ir
import Kit.Str
import Kit.Compiler.Generators.C

showstruct x = intercalate
  "\n"
  [ renderStyle (Style {mode = LeftMode}) $ pretty $ xi | xi <- cTypeDecl x ]
showdisc name variantNames =
  renderStyle (Style {mode = LeftMode}) $ pretty $ enumDiscriminant
    (Just name)
    [ ([], v) | v <- variantNames ]

spec :: Spec
spec = do
  describe "Transpiles type declarations" $ do
    it "Transpiles structs" $ do
      showstruct
          (newTypeDefinition
            { typeName    = ([], "MyStruct")
            , typeSubtype = Struct
              { structFields = [ newVarDefinition { varName = ([], "a")
                                                  , varType = BasicTypeInt 8
                                                  }
                               , newVarDefinition { varName = ([], "b")
                                                  , varType = BasicTypeUint 8
                                                  }
                               ]
              }
            }
          )
        `shouldBe` "struct MyStruct {\nint8_t a; uint8_t b;\n}"

    it "Transpiles enum discriminants" $ do
      showdisc "Apple" ["GrannySmith", "GoldenDelicious", "RedDelicious"]
        `shouldBe` "enum Apple {\nGrannySmith, GoldenDelicious, RedDelicious\n}"

    it "Transpiles simple enums" $ do
      showstruct
          (newTypeDefinition
            { typeName    = ([], "MyEnum")
            , typeSubtype = Enum
              { enumVariants = [ newEnumVariant { variantName = ([], "Option1")
                                                }
                               , newEnumVariant
                                 { variantName = ([], "AnotherOption")
                                 }
                               , newEnumVariant
                                 { variantName = ([], "TheThirdOption")
                                 }
                               ]
              }
            }
          )
        `shouldBe` "enum MyEnum {\nOption1, AnotherOption, TheThirdOption\n}"

    it "Transpiles complex enums" $ do
      shouldBe
        (showstruct
          (newTypeDefinition
            { typeName    = ([], "MyEnum")
            , typeSubtype = Enum
              { enumVariants = [ newEnumVariant
                                 { variantName = (["MyEnum"], "Variant1")
                                 }
                               , newEnumVariant
                                 { variantName = (["MyEnum"], "Variant2")
                                 , variantArgs = [ newArgSpec
                                                   { argName = "field1"
                                                   , argType = BasicTypeCChar
                                                   }
                                                 , newArgSpec
                                                   { argName = "field2"
                                                   , argType = BasicTypeUint 8
                                                   }
                                                 ]
                                 }
                               ]
              }
            }
          )
        )
        "struct MyEnum_Variant2__data {\nchar field1; uint8_t field2;\n}\n\
        \enum MyEnum_Discriminant {\nMyEnum__Variant1, MyEnum__Variant2\n}\n\
        \struct MyEnum {\nenum MyEnum_Discriminant __dsc;\nunion {\nstruct MyEnum_Variant2__data variant_Variant2;\n} __var;\n}"

