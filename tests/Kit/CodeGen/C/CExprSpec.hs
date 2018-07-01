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

  showctype x = intercalate " " [show $ pretty t | t <- ctype x]
  showexpr x = renderStyle (Style {mode = LeftMode}) $ pretty $ transpile_expr x
  showstmt x = renderStyle (Style {mode = LeftMode}) $ pretty $ transpile_stmt x
  showblock x = renderStyle (Style {mode = LeftMode}) $ pretty $ transpile_stmt $ IrBlock x

  spec :: Spec
  spec = do
    describe "Transpile basic types" $ do
      it "transpiles int types" $ do
        showctype (TypeInt 8) `shouldBe` "signed char"
        showctype (TypeInt 16) `shouldBe` "signed short"
        showctype (TypeInt 32) `shouldBe` "signed long"
        showctype (TypeInt 64) `shouldBe` "signed long long"
      it "transpiles unsigned int types" $ do
        showctype (TypeUint 8) `shouldBe` "unsigned char"
        showctype (TypeUint 16) `shouldBe` "unsigned short"
        showctype (TypeUint 32) `shouldBe` "unsigned long"
        showctype (TypeUint 64) `shouldBe` "unsigned long long"
      it "transpiles float types" $ do
        showctype (TypeFloat 32) `shouldBe` "float"
        showctype (TypeFloat 64) `shouldBe` "double"
      it "transpiles void types" $ do
        showctype TypeVoid `shouldBe` "void"
      it "transpiles atom types" $ do
        showctype (TypeAtom "MyAtom") `shouldBe` "unsigned long"
      it "transpiles enum types" $ do
        showctype (TypeSimpleEnum "MyEnum" []) `shouldBe` "MyEnum"
        showctype (TypeComplexEnum "MyEnum2" []) `shouldBe` "MyEnum2"

    describe "Transpile statements" $ do
      it "transpiles return statements" $ do
        showstmt (IrReturn Nothing) `shouldBe` "return;"
        showstmt (IrReturn $ Just $ IrIdentifier "abc") `shouldBe` "return abc;"
      it "transpiles continue and break" $ do
        showstmt (IrContinue) `shouldBe` "continue;"
        showstmt (IrBreak) `shouldBe` "break;"
      it "transpiles statement blocks" $ do
        showstmt (IrBlock [IrContinue, IrBreak]) `shouldBe` "{\ncontinue;\nbreak;\n}"
      it "transpiles if statements" $ do
        showstmt (IrIf (IrLiteral $ BoolValue True) (IrContinue) (Just $ IrBreak)) `shouldBe` "if (1)\n{\ncontinue;\n}\nelse\n{\nbreak;\n}"
        showstmt (IrIf (IrLiteral $ BoolValue True) (IrContinue) (Nothing)) `shouldBe` "if (1)\n{\ncontinue;\n}"
      it "transpiles local variable declarations" $ do
        showblock [
            IrVarDeclaration "my_var" (TypeUint 16) (Just $ IrIdentifier "a"),
            IrVarDeclaration "my_var2" (TypeFloat 32) Nothing
          ] `shouldBe` "{\nunsigned short my_var = a;\nfloat my_var2;\n}"
      it "transpiles while loops" $ do
        showstmt (IrWhile (IrIdentifier "a") (IrContinue)) `shouldBe` "while (a)\ncontinue;"
      it "transpiles for loops" $ do
        showstmt (IrFor "a" (TypeUint 8) (IrLiteral $ IntValue "1") (IrLiteral $ IntValue "5") (IrContinue)) `shouldBe` "for (unsigned char a = 1; a < 5; ++a)\ncontinue;"

    describe "Transpile expressions" $ do
      it "transpiles identifiers" $ do
        showexpr (IrIdentifier "apple_banana") `shouldBe` "apple_banana"
      it "transpiles bool literals" $ do
        showexpr (IrLiteral $ BoolValue False) `shouldBe` "0"
        showexpr (IrLiteral $ BoolValue True) `shouldBe` "1"
      it "transpiles int literals" $ do
        showexpr (IrLiteral $ IntValue "1") `shouldBe` "1"
        showexpr (IrLiteral $ IntValue "0100") `shouldBe` "100"
        showexpr (IrLiteral $ IntValue "0x1234") `shouldBe` "4660"
        showexpr (IrLiteral $ IntValue "0b0") `shouldBe` "0"
        showexpr (IrLiteral $ IntValue "0b1") `shouldBe` "1"
        showexpr (IrLiteral $ IntValue "0b011001") `shouldBe` "25"
        showexpr (IrLiteral $ IntValue "0o1732") `shouldBe` "986"
      it "transpiles float literals" $ do
        showexpr (IrLiteral $ FloatValue "0.1") `shouldBe` "0.1"
      it "transpiles binary operations" $ do
        showexpr (IrBinop Add (IrIdentifier "a") (IrIdentifier "b")) `shouldBe` "a + b"
        showexpr (IrBinop Sub (IrIdentifier "a") (IrIdentifier "b")) `shouldBe` "a - b"
        showexpr (IrBinop Mod (IrIdentifier "a") (IrIdentifier "b")) `shouldBe` "a % b"
        showexpr (IrBinop LeftShift (IrIdentifier "a") (IrIdentifier "b")) `shouldBe` "a << b"
        showexpr (IrBinop And (IrIdentifier "a") (IrIdentifier "b")) `shouldBe` "a && b"
        showexpr (IrBinop BitAnd (IrIdentifier "a") (IrIdentifier "b")) `shouldBe` "a & b"
        showexpr (IrBinop BitXor (IrIdentifier "a") (IrIdentifier "b")) `shouldBe` "a ^ b"
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
        showexpr (IrArrayAccess (IrIdentifier "a") (IrIdentifier "b")) `shouldBe` "a[b]"
      it "transpiles function calls" $ do
        showexpr (IrCall (IrIdentifier "a") [(IrIdentifier "b"), (IrIdentifier "c")]) `shouldBe` "a(b, c)"
      it "transpiles casts" $ do
        showexpr (IrCast (IrIdentifier "a") TypeVoid) `shouldBe` "(void) a"
        showexpr (IrCast (IrIdentifier "abc") (TypeInt 8)) `shouldBe` "(signed char) abc"
      {-it "transpiles vector literals" $ do
        showexpr (VectorLiteral [(IrIdentifier "b"), (IrIdentifier "c")]) `shouldBe` "[b, c]"-}

      -- TODO: vector literal
