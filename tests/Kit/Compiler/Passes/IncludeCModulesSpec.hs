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
    parseCHeader ctx "tests/Kit/Compiler/Passes/test_header.h"

  externVarDef s = newVarDefinition

  externFunctionDef s = newFunctionDefinition {
    function_name = s,
    function_meta = [metaExtern]
  }

  spec :: Spec
  spec = do
    describe "Parses C type declarations" $ do
      forM_ [
          BasicTypeVoid, BasicTypeBool,
          BasicTypeInt 8, BasicTypeInt 16, BasicTypeInt 32, BasicTypeInt 64,
          BasicTypeUint 8, BasicTypeUint 16, BasicTypeUint 32, BasicTypeUint 64
        ] (\t -> it ("Parses C specifiers into " ++ show t) $ parseDeclSpec (ctype t) `shouldBe` Just (TypeBasicType t))

      {-it "Resolves specifiers for structs into struct types" $ do
        parseDeclSpec (ctype (BasicTypeStruct ("mystruct", [("a", BasicTypeInt 8), ("b", BasicTypeUint 16)]))) `shouldBe` Just (TypeStruct [] "mystruct")
        parseDeclSpec (ctype (BasicTypeComplexEnum "myenum" [])) `shouldBe` Just (TypeStruct [] "myenum")

      it "Resolves specifiers for basic enums into enum types" $ do
        parseDeclSpec (ctype (BasicTypeSimpleEnum "myenum" [])) `shouldBe` Just (TypeEnum [] "myenum")-}

    describe "Parses C headers" $ do
      it "Parses test_header.h" $ do
        header <- testHeader
        True `shouldBe` True

      forM_ [
          ("Parses var declarations", "var1", VarBinding (ConcreteType $ TypeBasicType $ BasicTypeInt 16)),
          ("Parses var definitions", "var2", VarBinding (ConcreteType $ TypeBasicType $ BasicTypeInt 8)),
          ("Parses var definitions with multiple type specifiers", "var3", VarBinding (ConcreteType $ TypeBasicType $ BasicTypeUint 32)),
          ("Parses struct vars", "struct_var1", VarBinding (ConcreteType $ TypeStruct ([], "Struct2"))),
          ("Parses enum vars", "enum_var1", VarBinding (ConcreteType $ TypeEnum ([], "Enum1"))),
          ("Parses pointer vars", "pointer_var1", VarBinding (ConcreteType $ TypePtr (TypeBasicType $ BasicTypeInt 16))),
          ("Parses pointers to pointer vars", "pointer_var2", VarBinding (ConcreteType $ TypePtr (TypePtr (TypeBasicType $ BasicTypeInt 16)))),
          ("Parses function pointer vars", "pointer_var3", VarBinding (ConcreteType $ TypePtr (TypeFunction (TypeBasicType $ BasicTypeInt 16) [("arg1", TypeBasicType $ BasicTypeInt 16)] False))),
          ("Parses void functions", "void_func1", FunctionBinding (ConcreteType $ TypeBasicType $ BasicTypeVoid) [] False),
          ("Parses functions with non-void types", "int_func1", FunctionBinding (ConcreteType $ TypeBasicType $ BasicTypeInt 16) [] False),
          ("Parses functions with arguments", "func_with_args", FunctionBinding (ConcreteType $ TypeBasicType $ BasicTypeFloat 32) [
              ("arg1", ConcreteType $ TypeBasicType $ BasicTypeInt 16),
              ("arg2", ConcreteType $ TypeBasicType $ BasicTypeUint 64)
            ] False),
          ("Parses functions with struct return value/arguments", "struct_func", FunctionBinding (ConcreteType $ TypeStruct ([], "Struct1")) [
              ("a", ConcreteType $ TypeStruct ([], "Struct2"))
            ] False),
          ("Parses functions with pointer return value/arguments", "pointer_func", FunctionBinding (ConcreteType $ TypePtr $ TypeBasicType $ BasicTypeFloat 32) [
              ("arg1", ConcreteType $ TypePtr $ TypeBasicType $ BasicTypeInt 16)
            ] False),
          ("Parses variadic functions", "varargs_func", FunctionBinding (ConcreteType $ TypeBasicType $ BasicTypeVoid) [
              ("a", ConcreteType $ TypeBasicType $ BasicTypeInt 16)
            ] True)
        ] (\(label, name, val) -> it label $ do
            header <- testHeader
            binding <- resolveLocal (mod_vars header) name
            binding `shouldBe` Just (val))

      forM_ [
          ("Parses struct declarations", "Struct1", (newTypeDefinition "Struct1") {
            type_type = Struct {
              struct_fields = [
                newVarDefinition {var_name = Var "field1", var_type = Just $ ConcreteType $ TypeBasicType $ BasicTypeInt 8},
                newVarDefinition {var_name = Var "field2", var_type = Just $ ConcreteType $ TypeBasicType $ BasicTypeUint 16}
              ]
            }
          }),
          ("Parses struct typedefs", "Struct2", (newTypeDefinition "Struct2") {
            type_type = Struct {
              struct_fields = [
                newVarDefinition {var_name = Var "field1", var_type = Just $ ConcreteType $ TypeBasicType $ BasicTypeInt 16},
                newVarDefinition {var_name = Var "field2", var_type = Just $ ConcreteType $ TypeBasicType $ BasicTypeFloat 64}
              ]
            }
          }),
          ("Parses empty struct typedefs", "Struct3", (newTypeDefinition "Struct3") {
            type_type = Struct {struct_fields = []}
          }),
          ("Parses enum definitions", "Enum1", (newTypeDefinition "Enum1") {
            type_type = Enum {enum_variants = [
              newEnumVariant {variant_name = "apple"},
              newEnumVariant {variant_name = "banana"},
              newEnumVariant {variant_name = "strawberry"}
            ], enum_underlying_type = Nothing}
          })
        ] (\(label, name, val) -> it label $ do
            header <- testHeader
            binding <- resolveLocal (mod_types header) name
            let binding' = case binding of
                             Just (TypeUsage {type_definition = t}) -> Just t
                             Nothing -> Nothing
            binding' `shouldBe` Just val)
