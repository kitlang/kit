{-# OPTIONS_GHC -w #-}

module Kit.CodeGen.C.CExprSpec where

import Test.Hspec
import Test.QuickCheck
import Text.PrettyPrint
import Language.C
import Data.List
import Kit.Ast
import Kit.Ir
import Kit.Str
import Kit.CodeGen.C

showctype x = intercalate " " [ show $ pretty t | t <- fst $ ctype x ]
showexpr x = renderStyle (Style {mode = LeftMode}) $ pretty $ transpileExpr x
showstmt x = renderStyle (Style {mode = LeftMode}) $ pretty $ transpileStmt x
showblock x =
  renderStyle (Style {mode = LeftMode}) $ pretty $ transpileStmt $ IrBlock x

spec :: Spec
spec = do
  describe "Transpile basic types" $ do
    it "transpiles bool types" $ do
      showctype (BasicTypeBool) `shouldBe` "_Bool"
    it "transpiles int types" $ do
      showctype (BasicTypeInt 8) `shouldBe` "int8_t"
      showctype (BasicTypeInt 16) `shouldBe` "int16_t"
      showctype (BasicTypeInt 32) `shouldBe` "int32_t"
      showctype (BasicTypeInt 64) `shouldBe` "int64_t"
    it "transpiles unsigned int types" $ do
      showctype (BasicTypeUint 8) `shouldBe` "uint8_t"
      showctype (BasicTypeUint 16) `shouldBe` "uint16_t"
      showctype (BasicTypeUint 32) `shouldBe` "uint32_t"
      showctype (BasicTypeUint 64) `shouldBe` "uint64_t"
    it "transpiles float types" $ do
      showctype (BasicTypeFloat 32) `shouldBe` "float"
      showctype (BasicTypeFloat 64) `shouldBe` "double"
    it "transpiles void types" $ do
      showctype BasicTypeVoid `shouldBe` "void"
    it "transpiles atom types" $ do
      showctype BasicTypeAtom `shouldBe` "unsigned long"
    it "transpiles basic enum types" $ do
      showctype (BasicTypeSimpleEnum (Just "MyEnum") []) `shouldBe` "enum MyEnum"
    it "transpiles complex enum types" $ do
      showctype (BasicTypeComplexEnum "MyEnum2" []) `shouldBe` "struct MyEnum2"
    it "transpiles struct types" $ do
      showctype (BasicTypeStruct (Just "abc") []) `shouldBe` "struct abc"

  describe "Transpile statements" $ do
    it "transpiles return statements" $ do
      showstmt (IrReturn Nothing) `shouldBe` "return;"
      showstmt (IrReturn $ Just $ IrIdentifier "abc") `shouldBe` "return abc;"
    it "transpiles continue and break" $ do
      showstmt (IrContinue) `shouldBe` "continue;"
      showstmt (IrBreak) `shouldBe` "break;"
    it "transpiles statement blocks" $ do
      showstmt (IrBlock [IrContinue, IrBreak])
        `shouldBe` "{\ncontinue;\nbreak;\n}"
    it "transpiles if statements" $ do
      showstmt
          (IrIf (IrLiteral $ BoolValue True) (IrContinue) (Just $ IrBreak))
        `shouldBe` "if (1)\n{\ncontinue;\n}\nelse\n{\nbreak;\n}"
      showstmt (IrIf (IrLiteral $ BoolValue True) (IrContinue) (Nothing))
        `shouldBe` "if (1)\n{\ncontinue;\n}"
    it "transpiles local variable declarations" $ do
      showblock
          [ IrVarDeclaration "my_var"
                             (BasicTypeUint 16)
                             (Just $ IrIdentifier "a")
          , IrVarDeclaration "my_var2" (BasicTypeFloat 32) Nothing
          ]
        `shouldBe` "{\nuint16_t my_var = a;\nfloat my_var2;\n}"
    it "transpiles while loops" $ do
      showstmt (IrWhile (IrIdentifier "a") (IrContinue) False)
        `shouldBe` "while (a)\ncontinue;"
    it "transpiles do-while loops" $ do
      showstmt (IrWhile (IrIdentifier "a") (IrContinue) True)
        `shouldBe` "do\ncontinue;\nwhile (a);"
    it "transpiles for loops" $ do
      showstmt
          (IrFor "a"
                 (BasicTypeUint 8)
                 (IrLiteral $ IntValue 1 $ BasicTypeInt 8)
                 (IrLiteral $ IntValue 5 $ BasicTypeInt 8)
                 (IrContinue)
          )
        `shouldBe` "for (uint8_t a = 1; a < 5; ++a)\ncontinue;"

  describe "Transpile expressions" $ do
    it "transpiles identifiers" $ do
      showexpr (IrIdentifier "apple_banana") `shouldBe` "apple_banana"
    it "transpiles bool literals" $ do
      showexpr (IrLiteral $ BoolValue False) `shouldBe` "0"
      showexpr (IrLiteral $ BoolValue True) `shouldBe` "1"
    it "transpiles int literals" $ do
      showexpr (IrLiteral $ IntValue 1 $ BasicTypeInt 16) `shouldBe` "1"
      showexpr (IrLiteral $ IntValue 1234 $ BasicTypeInt 32) `shouldBe` "1234L"
      showexpr (IrLiteral $ IntValue (-50) $ BasicTypeInt 8) `shouldBe` "-50"
    it "transpiles float literals" $ do
      showexpr (IrLiteral $ FloatValue "0.1" $ BasicTypeFloat 32) `shouldBe` "0.1"
    it "transpiles binary operations" $ do
      showexpr (IrBinop Add (IrIdentifier "a") (IrIdentifier "b"))
        `shouldBe` "a + b"
      showexpr (IrBinop Sub (IrIdentifier "a") (IrIdentifier "b"))
        `shouldBe` "a - b"
      showexpr (IrBinop Mod (IrIdentifier "a") (IrIdentifier "b"))
        `shouldBe` "a % b"
      showexpr (IrBinop LeftShift (IrIdentifier "a") (IrIdentifier "b"))
        `shouldBe` "a << b"
      showexpr (IrBinop And (IrIdentifier "a") (IrIdentifier "b"))
        `shouldBe` "a && b"
      showexpr (IrBinop BitAnd (IrIdentifier "a") (IrIdentifier "b"))
        `shouldBe` "a & b"
      showexpr (IrBinop BitXor (IrIdentifier "a") (IrIdentifier "b"))
        `shouldBe` "a ^ b"
    it "transpiles prefix unary operations" $ do
      showexpr (IrPreUnop Sub (IrIdentifier "a")) `shouldBe` "-a"
      showexpr (IrPreUnop Invert (IrIdentifier "a")) `shouldBe` "!a"
      showexpr (IrPreUnop InvertBits (IrIdentifier "a")) `shouldBe` "~a"
      showexpr (IrPreUnop Inc (IrIdentifier "a")) `shouldBe` "++a"
      showexpr (IrPreUnop Dec (IrIdentifier "a")) `shouldBe` "--a"
    it "transpiles postfix unary operations" $ do
      showexpr (IrPostUnop Inc (IrIdentifier "a")) `shouldBe` "a++"
      showexpr (IrPostUnop Dec (IrIdentifier "a")) `shouldBe` "a--"
    it "transpiles field access" $ do
      showexpr (IrField (IrIdentifier "a") "abc") `shouldBe` "a.abc"
    it "transpiles array access" $ do
      showexpr (IrArrayAccess (IrIdentifier "a") (IrIdentifier "b"))
        `shouldBe` "a[b]"
    it "transpiles function calls" $ do
      showexpr
          (IrCall (IrIdentifier "a") [(IrIdentifier "b"), (IrIdentifier "c")])
        `shouldBe` "a(b, c)"
    it "transpiles casts" $ do
      showexpr (IrCast (IrIdentifier "a") BasicTypeVoid) `shouldBe` "(void) a"
      showexpr (IrCast (IrIdentifier "abc") (BasicTypeInt 8))
        `shouldBe` "(int8_t) abc"
    {-it "transpiles vector literals" $ do
      showexpr (VectorLiteral [(IrIdentifier "b"), (IrIdentifier "c")]) `shouldBe` "[b, c]"-}

    -- TODO: vector literal
