module Kit.Compiler.Passes.IncludeCModulesSpec where

import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Kit.Ast
import Kit.CodeGen.C
import Kit.Compiler
import Kit.Compiler.Passes
import Kit.Ir

testHeader :: IO Module
testHeader = do
  ctx <- newCompileContext
  -- let ctx = ctx' {
  --   ctxIncludePaths = ["tests/Kit/Compiler/Passes"]
  -- }
  let testHeader = "tests/Kit/Compiler/Passes/test_header.h"
  mod <- newCMod ""
  parseCHeader ctx mod testHeader
  return mod

externVarDef s = newVarDefinition

externFunctionDef s =
  newFunctionDefinition { functionName = s, functionMeta = [metaExtern] }

spec :: Spec
spec = do
  describe "Parses C type declarations" $ do
    forM_
      [ BasicTypeVoid
      , BasicTypeBool
      , BasicTypeInt 8
      , BasicTypeInt 16
      , BasicTypeInt 32
      , BasicTypeInt 64
      , BasicTypeUint 8
      , BasicTypeUint 16
      , BasicTypeUint 32
      , BasicTypeUint 64
      ]
      (\t ->
        it ("Parses C specifiers into " ++ show t)
          $          parseType [] (fst $ ctype t) []
          `shouldBe` TypeBasicType t
      )

    {-it "Resolves specifiers for structs into struct types" $ do
      parseDeclSpec (ctype (BasicTypeStruct ("mystruct", [("a", BasicTypeInt 8), ("b", BasicTypeUint 16)]))) `shouldBe` Just (TypeStruct [] "mystruct")
      parseDeclSpec (ctype (BasicTypeComplexEnum "myenum" [])) `shouldBe` Just (TypeStruct [] "myenum")

    it "Resolves specifiers for basic enums into enum types" $ do
      parseDeclSpec (ctype (BasicTypeSimpleEnum "myenum" [])) `shouldBe` Just (TypeEnum [] "myenum")-}

  describe "Parses C headers" $ do
    it "Parses test_header.h" $ do
      header <- testHeader
      True `shouldBe` True

    forM_
      [ ( "Parses var declarations"
        , "var1"
        , VarBinding (TypeBasicType $ BasicTypeInt 16)
        )
      , ( "Parses var definitions"
        , "var2"
        , VarBinding (TypeBasicType $ BasicTypeInt 8)
        )
      , ( "Parses var definitions with multiple type specifiers"
        , "var3"
        , VarBinding (TypeBasicType $ BasicTypeUint 32)
        )
      , ( "Parses struct vars"
        , "struct_var1"
        , VarBinding (TypeStruct (["c"], "Struct1") [])
        )
      , ( "Parses enum vars"
        , "enum_var1"
        , VarBinding (TypeEnum (["c"], "Enum1") [])
        )
      , ( "Parses pointer vars"
        , "pointer_var1"
        , VarBinding (TypePtr (TypeBasicType $ BasicTypeInt 16))
        )
      , ( "Parses pointers to pointer vars"
        , "pointer_var2"
        , VarBinding (TypePtr (TypePtr (TypeBasicType $ BasicTypeInt 16)))
        )
      , ( "Parses function pointer vars"
        , "pointer_var3"
        , VarBinding
          (TypePtr
            (TypeFunction (TypeBasicType $ BasicTypeInt 16)
                          [("arg1", TypeBasicType $ BasicTypeInt 16)]
                          False
            )
          )
        )
      , ( "Parses void functions"
        , "void_func1"
        , FunctionBinding (TypeBasicType $ BasicTypeVoid) [] False
        )
      , ( "Parses functions with non-void types"
        , "int_func1"
        , FunctionBinding (TypeBasicType $ BasicTypeInt 16) [] False
        )
      , ( "Parses functions with arguments"
        , "func_with_args"
        , FunctionBinding
          (TypeBasicType $ BasicTypeFloat 32)
          [ ("arg1", TypeBasicType $ BasicTypeInt 16)
          , ("arg2", TypeBasicType $ BasicTypeUint 64)
          ]
          False
        )
      , ( "Parses functions with struct return value/arguments"
        , "struct_func"
        , FunctionBinding (TypeStruct (["c"], "Struct1") [])
                          [("a", TypeStruct (["c"], "Struct2") [])]
                          False
        )
      , ( "Parses functions with pointer return value/arguments"
        , "pointer_func"
        , FunctionBinding
          (TypePtr $ TypeBasicType $ BasicTypeFloat 32)
          [("arg1", TypePtr $ TypeBasicType $ BasicTypeInt 16)]
          False
        )
      , ( "Parses variadic functions"
        , "varargs_func"
        , FunctionBinding (TypeBasicType $ BasicTypeVoid)
                          [("a", TypeBasicType $ BasicTypeInt 16)]
                          True
        )
      , ( "Parses void arg functions"
        , "void_func"
        , FunctionBinding (TypeBasicType $ BasicTypeInt 32) [] False
        )
      ]
      (\(label, name, val) -> it label $ do
        header  <- testHeader
        binding <- resolveLocal (modVars header) name
        binding `shouldBe` Just (newBinding val Nothing)
      )

    forM_
      [ ( "Parses struct declarations"
        , "Struct1"
        , TypeStruct (["c"], "Struct1") []
        , Just $ (newTypeDefinition "Struct1")
          { typeNameMangling = Nothing
          , typeType         = Struct
            { struct_fields = [ newVarDefinition
                                { varName         = "field1"
                                , varNameMangling = Nothing
                                , varType         = Just
                                  $ ConcreteType
                                  $ TypeBasicType
                                  $ BasicTypeInt 8
                                }
                              , newVarDefinition
                                { varName         = "field2"
                                , varNameMangling = Nothing
                                , varType         = Just
                                  $ ConcreteType
                                  $ TypeBasicType
                                  $ BasicTypeUint 16
                                }
                              ]
            }
          }
        )
      , ( "Parses unnamed struct typedefs"
        , "Struct2"
        , TypeAnonStruct
          [ ("field1", TypeBasicType $ BasicTypeInt 16)
          , ("field2", TypeBasicType $ BasicTypeFloat 64)
          ]
        , Nothing
        )
      , ( "Parses empty struct typedefs"
        , "Struct3"
        , TypeStruct (["c"], "Struct3") []
        , Just $ (newTypeDefinition "Struct3") { typeNameMangling = Nothing
                                               , typeType         = Struct
                                                 { struct_fields = []
                                                 }
                                               }
        )
      , ( "Parses enum definitions"
        , "Enum1"
        , TypeEnum (["c"], "Enum1") []
        , Just $ (newTypeDefinition "Enum1")
          { typeNameMangling = Nothing
          , typeType         = Enum
            { enum_variants = [ newEnumVariant { variantName = "apple" }
                              , newEnumVariant { variantName = "banana" }
                              , newEnumVariant { variantName = "strawberry" }
                              ]
            , enum_underlying_type = Nothing
            }
          }
        )
      ]
      (\(label, name, ct, val) -> it label $ do
        header <- testHeader
        x      <- resolveLocal (modTypes header) name
        x `shouldBe` Just ct
        binding <- resolveLocal (modTypeDefinitions header) name
        binding `shouldBe` val
      )
