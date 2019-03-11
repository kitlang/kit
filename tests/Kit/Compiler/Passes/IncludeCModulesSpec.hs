module Kit.Compiler.Passes.IncludeCModulesSpec where

import Control.Monad
import Data.Maybe
import Language.C
import Test.Hspec
import Test.QuickCheck
import Kit.Ast
import Kit.Compiler
import Kit.Compiler.Generators.C
import Kit.Compiler.Ir
import Kit.Compiler.Passes
import Kit.HashTable
import Kit.Ir
import Kit.Toolchain

concreteArgs = map (\(n, t) -> newArgSpec { argName = n, argType = t })

testHeader :: IO (CompileContext, Module)
testHeader = do
  ctx <- newCompileContext
  cc  <- loadToolchain defaultToolchain
  let testHeader = "tests/Kit/Compiler/Passes/test_header.h"
  mod <- newCMod
  parseCHeader ctx cc mod testHeader
  return (ctx, mod)

externVarDef s = newVarDefinition { varName = s, varMeta = [meta metaExtern] }

externFunctionDef s =
  newFunctionDefinition { functionName = s, functionMeta = [meta metaExtern] }

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
      , BasicTypeCChar
      , BasicTypeCInt
      , BasicTypeCSize
      ]
      (\t -> it ("Parses C specifiers into " ++ show t) $ do
        ctx <- newCompileContext
        mod <- newMod [] ""
        t'  <- findUnderlyingType ctx mod Nothing
          $ parseType [] (fst $ ctype t) []
        t' `shouldBe` t
      )

    {-it "Resolves specifiers for structs into struct types" $ do
      parseDeclSpec (ctype (BasicTypeStruct ("mystruct", [("a", BasicTypeInt 8), ("b", BasicTypeUint 16)]))) `shouldBe` Just (TypeStruct [] "mystruct")
      parseDeclSpec (ctype (BasicTypeComplexEnum "myenum" [])) `shouldBe` Just (TypeStruct [] "myenum")

    it "Resolves specifiers for basic enums into enum types" $ do
      parseDeclSpec (ctype (BasicTypeSimpleEnum "myenum" [])) `shouldBe` Just (TypeEnum [] "myenum")-}

  describe "Parses C headers" $ do
    it "Parses test_header.h" $ do
      (ctx, header) <- testHeader
      True `shouldBe` True

    forM_
      [ ("Parses var declarations", "var1", (TypeInt 16))
      , ("Parses var definitions" , "var2", (TypeChar))
      , ( "Parses var definitions with multiple type specifiers"
        , "var3"
        , (TypeUint 32)
        )
      , ("Parses struct vars", "struct_var1", (TypeInstance ([], "Struct1") []))
      , ("Parses enum vars"   , "enum_var1"   , (TypeInstance ([], "Enum1") []))
      , ("Parses pointer vars", "pointer_var1", (TypePtr (TypeInt 16)))
      , ( "Parses pointers to pointer vars"
        , "pointer_var2"
        , (TypePtr (TypePtr (TypeInt 16)))
        )
      , ( "Parses function pointer vars"
        , "pointer_var3"
        , (TypePtr
            (TypeFunction (TypeInt 16)
                          (concreteArgs [("arg1", TypeInt 16)])
                          Nothing
                          []
            )
          )
        )
      , ( "Parses void function pointers"
        , "func_pointer"
        , (TypePtr $ TypeFunction TypeVoid (concreteArgs []) Nothing [])
        )
      , ( "Parses void functions"
        , "void_func1"
        , TypeFunction TypeVoid (concreteArgs []) Nothing []
        )
      , ( "Parses extern inline functions"
        , "extern_inline_void_func"
        , TypeFunction TypeVoid (concreteArgs []) Nothing []
        )
      , ( "Parses static inline functions"
        , "static_inline_void_func"
        , TypeFunction TypeVoid (concreteArgs []) Nothing []
        )
      , ( "Parses function definitions"
        , "defined_function"
        , TypeFunction (TypeInt 0) (concreteArgs [("arg1", TypePtr $ TypeInt 0)]) (Just "") []
        )
      , ( "Parses functions with non-void types"
        , "int_func1"
        , TypeFunction (TypeInt 16) (concreteArgs []) Nothing []
        )
      , ( "Parses functions with arguments"
        , "func_with_args"
        , TypeFunction
          (TypeFloat 32)
          (concreteArgs [("arg1", TypeInt 16), ("arg2", TypeUint 64)])
          Nothing
          []
        )
      , ( "Parses functions with struct return value/arguments"
        , "struct_func"
        , TypeFunction (TypeInstance ([], "Struct1") [])
                       (concreteArgs [("a", TypeInstance ([], "Struct2") [])])
                       Nothing
                       []
        )
      , ( "Parses functions with pointer return value/arguments"
        , "pointer_func"
        , TypeFunction (TypePtr $ TypeFloat 32)
                       (concreteArgs [("arg1", TypePtr $ TypeInt 0)])
                       Nothing
                       []
        )
      , ( "Parses variadic functions"
        , "varargs_func"
        , TypeFunction TypeVoid (concreteArgs [("a", TypeInt 16)]) (Just "") []
        )
      , ( "Parses void arg functions"
        , "void_func"
        , TypeFunction (TypeInt 32) (concreteArgs []) Nothing []
        )
      , ( "Parses atexit"
        , "fake_atexit"
        , TypeFunction
          (TypeInt 0)
          (concreteArgs
            [ ( "__func"
              , TypePtr $ TypeFunction TypeVoid (concreteArgs []) Nothing []
              )
            ]
          )
          Nothing
          []
        )
      , ("Parses 'unsigned' by itself", "just_unsigned", (TypeUint 0))
      , ( "Parses pointer to const char"
        , "const_char"
        , TypePtr (TypeConst (TypeChar))
        )
      -- , ("Parses const pointer", "const_ptr", TypeConst (TypePtr (TypeInt 0)))
      , ( "Parses function pointer typedef"
        , "func_pointah"
        , TypeFunction (TypeTypedef "fp_typedef")
                       (concreteArgs [("arg3", TypeInt 0)])
                       Nothing
                       []
        )
      ]
      (\(label, name, ct) -> it label $ do
        (ctx, header) <- testHeader
        binding       <- getBinding ctx ([], name)
        (case binding of
            FunctionBinding f -> Just $ functionConcrete f
            VarBinding      v -> Just $ varType v
          )
          `shouldBe` Just ct
      )

    forM_
      [ ( "Parses struct declarations"
        , "Struct1"
        , TypeInstance ([], "Struct1") []
        , Just $ typeDecl $ (newTypeDefinition :: TypeDefinition Expr TypeSpec)
          { typeName    = ([], "Struct1")
          , typeSubtype = StructUnion
            { structUnionFields = [ newVarDefinition
                                    { varName = ([], "field1")
                                    , varType = ConcreteType $ TypeChar
                                    }
                                  , newVarDefinition
                                    { varName = ([], "field2")
                                    , varType = ConcreteType $ TypeUint 16
                                    }
                                  ]
            , isStruct          = True
            }
          }
        )
      , ( "Parses anonymous struct typedefs"
        , "Struct2"
        , TypeAnonStruct (Just "Struct2")
                         [("field1", TypeInt 16), ("field2", TypeFloat 64)]
        , Nothing
        )
      , ( "Parses empty struct typedefs"
        , "Struct3"
        , TypeInstance ([], "Struct3") []
        , Just $ typeDecl $ (newTypeDefinition :: TypeDefinition Expr TypeSpec)
          { typeName    = ([], "Struct3")
          , typeSubtype = StructUnion {structUnionFields = [], isStruct = True}
          }
        )
      , ( "Parses enum definitions"
        , "Enum1"
        , TypeInstance ([], "Enum1") []
        , Just $ typeDecl $ (newTypeDefinition :: TypeDefinition Expr TypeSpec)
          { typeName    = ([], "Enum1")
          , typeSubtype = Enum
            { enumVariants = [ newEnumVariant { variantName = ([], "apple") }
                             , newEnumVariant { variantName = ([], "banana") }
                             , newEnumVariant { variantName = ([], "cherry") }
                             ]
            , enumUnderlyingType = InferredType NoPos
            }
          }
        )
      , ( "Parses anonymous enum typedefs"
        , "Enum2"
        , TypeAnonEnum (Just "Enum2") ["kiwi", "lime", "mango"]
        , Nothing
        )
      ]
      (\(label, name, ct, def) -> it label $ do
        (ctx, header) <- testHeader
        x             <- lookupBinding ctx ([], name)
        result        <-
          (case x of
            Just (TypeBinding t) ->
              return $ Just $ TypeInstance (typeName t) []
            Nothing -> do
              x <- h_lookup (ctxTypedefs ctx) name
              case x of
                Just t -> return $ Just t
                _      -> return Nothing
            _ -> return Nothing
          )
        result `shouldBe` Just ct
      )
        -- def' <- h_lookup (modContents header) name
        -- def' `shouldBe` def
