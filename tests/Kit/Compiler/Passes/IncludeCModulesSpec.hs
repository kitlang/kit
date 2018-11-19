module Kit.Compiler.Passes.IncludeCModulesSpec where

import Control.Monad
import Data.Maybe
import Language.C
import Test.Hspec
import Test.QuickCheck
import Kit.Ast
import Kit.Compiler.Generators.C
import Kit.Compiler
import Kit.Compiler.Passes
import Kit.HashTable
import Kit.Ir

testHeader :: IO (CompileContext, Module)
testHeader = do
  ctx <- newCompileContext
  -- let ctx = ctx' {
  --   ctxIncludePaths = ["tests/Kit/Compiler/Passes"]
  -- }
  let testHeader = "tests/Kit/Compiler/Passes/test_header.h"
  mod <- newCMod
  parseCHeader ctx mod testHeader
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
      (\t ->
        it ("Parses C specifiers into " ++ show t)
          $          parseType
                       []
                       (fst
                         (let (a, b) = ctype t
                          in  ( catMaybes $ map
                                (\x -> case x of
                                  CTypeSpec x -> Just x
                                  _           -> Nothing
                                )
                                a
                              , b
                              )
                         )
                       )
                       []
          `shouldBe` TypeBasicType t
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
      [ let (n, t) = ("var1", (TypeBasicType $ BasicTypeInt 16))
        in  ("Parses var declarations", "var1", t)
      , ("Parses var definitions", "var2", (TypeBasicType $ BasicTypeCChar))
      , ( "Parses var definitions with multiple type specifiers"
        , "var3"
        , (TypeBasicType $ BasicTypeUint 32)
        )
      , ("Parses struct vars", "struct_var1", (TypeInstance ([], "Struct1") []))
      , ("Parses enum vars"  , "enum_var1"  , (TypeInstance ([], "Enum1") []))
      , ( "Parses pointer vars"
        , "pointer_var1"
        , (TypePtr (TypeBasicType $ BasicTypeInt 16))
        )
      , ( "Parses pointers to pointer vars"
        , "pointer_var2"
        , (TypePtr (TypePtr (TypeBasicType $ BasicTypeInt 16)))
        )
      , ( "Parses function pointer vars"
        , "pointer_var3"
        , (TypePtr
            (TypeFunction (TypeBasicType $ BasicTypeInt 16)
                          [("arg1", TypeBasicType $ BasicTypeInt 16)]
                          False
                          []
            )
          )
        )
      , ( "Parses void function pointers"
        , "func_pointer"
        , (TypePtr $ TypeFunction (TypeBasicType $ BasicTypeVoid) [] False [])
        )
      , ( "Parses void functions"
        , "void_func1"
        , TypeFunction (TypeBasicType $ BasicTypeVoid) [] False []
        )
      , ( "Parses functions with non-void types"
        , "int_func1"
        , TypeFunction (TypeBasicType $ BasicTypeInt 16) [] False []
        )
      , ( "Parses functions with arguments"
        , "func_with_args"
        , TypeFunction
          (TypeBasicType $ BasicTypeFloat 32)
          [ ("arg1", TypeBasicType $ BasicTypeInt 16)
          , ("arg2", TypeBasicType $ BasicTypeUint 64)
          ]
          False
          []
        )
      , ( "Parses functions with struct return value/arguments"
        , "struct_func"
        , TypeFunction (TypeInstance ([], "Struct1") [])
                       [("a", TypeInstance ([], "Struct2") [])]
                       False
                       []
        )
      , ( "Parses functions with pointer return value/arguments"
        , "pointer_func"
        , TypeFunction (TypePtr $ TypeBasicType $ BasicTypeFloat 32)
                       [("arg1", TypePtr $ TypeBasicType $ BasicTypeCInt)]
                       False
                       []
        )
      , ( "Parses variadic functions"
        , "varargs_func"
        , TypeFunction (TypeBasicType $ BasicTypeVoid)
                       [("a", TypeBasicType $ BasicTypeInt 16)]
                       True
                       []
        )
      , ( "Parses void arg functions"
        , "void_func"
        , TypeFunction (TypeBasicType $ BasicTypeInt 32) [] False []
        )
      , ( "Parses atexit"
        , "fake_atexit"
        , TypeFunction
          (TypeBasicType $ BasicTypeCInt)
          [ ( "__func"
            , TypePtr $ TypeFunction (TypeBasicType $ BasicTypeVoid) [] False []
            )
          ]
          False
          []
        )
      , ( "Parses 'unsigned' by itself"
        , "just_unsigned"
        , (TypeBasicType $ BasicTypeCUint)
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
        , Just $ DeclType $ (newTypeDefinition)
          { typeName    = ([], "Struct1")
          , typeSubtype = Struct
            { structFields = [ newVarDefinition
                               { varName = ([], "field1")
                               , varType = Just
                                 $ ConcreteType
                                 $ TypeBasicType
                                 $ BasicTypeCChar
                               }
                             , newVarDefinition
                               { varName = ([], "field2")
                               , varType = Just
                                 $ ConcreteType
                                 $ TypeBasicType
                                 $ BasicTypeUint 16
                               }
                             ]
            }
          }
        )
      , ( "Parses anonymous struct typedefs"
        , "Struct2"
        , TypeAnonStruct
          (Just "Struct2")
          [ ("field1", TypeBasicType $ BasicTypeInt 16)
          , ("field2", TypeBasicType $ BasicTypeFloat 64)
          ]
        , Nothing
        )
      , ( "Parses empty struct typedefs"
        , "Struct3"
        , TypeInstance ([], "Struct3") []
        , Just $ DeclType $ (newTypeDefinition)
          { typeName    = ([], "Struct3")
          , typeSubtype = Struct {structFields = []}
          }
        )
      , ( "Parses enum definitions"
        , "Enum1"
        , TypeInstance ([], "Enum1") []
        , Just $ DeclType $ (newTypeDefinition)
          { typeName    = ([], "Enum1")
          , typeSubtype = Enum
            { enumVariants = [ newEnumVariant { variantName = ([], "apple") }
                             , newEnumVariant { variantName = ([], "banana") }
                             , newEnumVariant { variantName = ([], "cherry") }
                             ]
            , enumUnderlyingType = Nothing
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
