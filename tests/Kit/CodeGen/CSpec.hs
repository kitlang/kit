module Kit.CodeGen.CSpec where

  import Test.Hspec
  import Test.QuickCheck
  import Text.PrettyPrint
  import Language.C
  import Data.List
  import Kit.Str
  import Kit.Ast.Expr
  import Kit.Ast.Operator
  import Kit.Ast.Type
  import Kit.Ast.Value
  import Kit.CodeGen.C

  showctype x = intercalate " " [show $ pretty t | t <- ctype x]
  showexpr x = renderStyle (Style {mode = LeftMode}) $ pretty $ transpile_expr x
  showstmt x = renderStyle (Style {mode = LeftMode}) $ pretty $ transpile_stmt x
  showblock x = renderStyle (Style {mode = LeftMode}) $ pretty $ transpile_stmt $ e $ Block x

  ident x = e $ Lvalue $ Var $ s_pack x

  spec :: Spec
  spec = do
    describe "Transpile basic types" $ do
      it "transpiles int types" $ do
        showctype (BasicType (TypeInt 8)) `shouldBe` "signed char"
        showctype (BasicType (TypeInt 16)) `shouldBe` "signed short"
        showctype (BasicType (TypeInt 32)) `shouldBe` "signed long"
        showctype (BasicType (TypeInt 64)) `shouldBe` "signed long long"
      it "transpiles unsigned int types" $ do
        showctype (BasicType (TypeUint 8)) `shouldBe` "unsigned char"
        showctype (BasicType (TypeUint 16)) `shouldBe` "unsigned short"
        showctype (BasicType (TypeUint 32)) `shouldBe` "unsigned long"
        showctype (BasicType (TypeUint 64)) `shouldBe` "unsigned long long"
      it "transpiles float types" $ do
        showctype (BasicType (TypeFloat 32)) `shouldBe` "float"
        showctype (BasicType (TypeFloat 64)) `shouldBe` "double"
      it "transpiles void types" $ do
        showctype (BasicType TypeVoid) `shouldBe` "void"

    describe "Transpile statements" $ do
      it "transpiles return statements" $ do
        showstmt (e $ Return Nothing) `shouldBe` "return;"
        showstmt (e $ Return $ Just $ ident "abc") `shouldBe` "return abc;"
      it "transpiles continue and break" $ do
        showstmt (e $ Continue) `shouldBe` "continue;"
        showstmt (e $ Break) `shouldBe` "break;"
      it "transpiles statement blocks" $ do
        showstmt (e $ Block [e $ Continue, e $ Break]) `shouldBe` "{\ncontinue;\nbreak;\n}"
      it "transpiles if statements" $ do
        showstmt (e $ If (e $ Literal $ BoolValue True) (e $ Continue) (Just $ e $ Break)) `shouldBe` "if (1)\n{\ncontinue;\n}\nelse\n{\nbreak;\n}"
        showstmt (e $ If (e $ Literal $ BoolValue True) (e $ Continue) (Nothing)) `shouldBe` "if (1)\n{\ncontinue;\n}"
      it "transpiles local variable declarations" $ do
        showblock [
            te (e $ VarDef $ VarDefinition {
              var_name = Var $ s_pack "my_var",
              var_default = Just $ ident "a"
            }) (BasicType (TypeUint 16)),
            te (e $ VarDef $ VarDefinition {
              var_name = Var $ s_pack "my_var2",
              var_default = Nothing
            }) (BasicType (TypeFloat 32))
          ] `shouldBe` "{\nunsigned short my_var = a;\nfloat my_var2;\n}"
      it "transpiles while loops" $ do
        showstmt (e $ While (ident "a") (e $ Continue)) `shouldBe` "while (a)\ncontinue;"
      it "transpiles for loops" $ do
        showstmt (e $ For (te (ident "a") (BasicType $ TypeUint 8)) (e $ RangeLiteral (e $ Literal $ IntValue $ s_pack "1") (e $ Literal $ IntValue $ s_pack "5")) (e $ Continue)) `shouldBe` "for (unsigned char a = 1; a < 5; ++a)\ncontinue;"

    describe "Transpile expressions" $ do
      it "transpiles identifiers" $ do
        showexpr (e $ Lvalue (Var $ s_pack "apple_banana")) `shouldBe` "apple_banana"
      it "transpiles bool literals" $ do
        showexpr (e $ Literal $ BoolValue False) `shouldBe` "0"
        showexpr (e $ Literal $ BoolValue True) `shouldBe` "1"
      it "transpiles int literals" $ do
        showexpr (e $ Literal $ IntValue $ s_pack "1") `shouldBe` "1"
        showexpr (e $ Literal $ IntValue $ s_pack "0100") `shouldBe` "100"
        showexpr (e $ Literal $ IntValue $ s_pack "0x1234") `shouldBe` "4660"
        showexpr (e $ Literal $ IntValue $ s_pack "0b0") `shouldBe` "0"
        showexpr (e $ Literal $ IntValue $ s_pack "0b1") `shouldBe` "1"
        showexpr (e $ Literal $ IntValue $ s_pack "0b011001") `shouldBe` "25"
        showexpr (e $ Literal $ IntValue $ s_pack "0o1732") `shouldBe` "986"
      it "transpiles float literals" $ do
        showexpr (e $ Literal $ FloatValue $ s_pack "0.1") `shouldBe` "0.1"
      it "transpiles binary operations" $ do
        showexpr (e $ Binop Add (ident "a") (ident "b")) `shouldBe` "a + b"
        showexpr (e $ Binop Sub (ident "a") (ident "b")) `shouldBe` "a - b"
        showexpr (e $ Binop Mod (ident "a") (ident "b")) `shouldBe` "a % b"
        showexpr (e $ Binop LeftShift (ident "a") (ident "b")) `shouldBe` "a << b"
        showexpr (e $ Binop And (ident "a") (ident "b")) `shouldBe` "a && b"
        showexpr (e $ Binop BitAnd (ident "a") (ident "b")) `shouldBe` "a & b"
        showexpr (e $ Binop BitXor (ident "a") (ident "b")) `shouldBe` "a ^ b"
      it "transpiles prefix unary operations" $ do
        showexpr (e $ PreUnop Sub (ident "a")) `shouldBe` "-a"
        showexpr (e $ PreUnop Invert (ident "a")) `shouldBe` "!a"
        showexpr (e $ PreUnop InvertBits (ident "a")) `shouldBe` "~a"
        showexpr (e $ PreUnop Inc (ident "a")) `shouldBe` "++a"
        showexpr (e $ PreUnop Dec (ident "a")) `shouldBe` "--a"
      it "transpiles postfix unary operations" $ do
        showexpr (e $ PostUnop Inc (ident "a")) `shouldBe` "a++"
        showexpr (e $ PostUnop Dec (ident "a")) `shouldBe` "a--"
      it "transpiles field access" $ do
        showexpr (e $ Field (ident "a") (s_pack "abc")) `shouldBe` "a.abc"
      it "transpiles array access" $ do
        showexpr (e $ ArrayAccess (ident "a") (ident "b")) `shouldBe` "a[b]"
      it "transpiles function calls" $ do
        showexpr (e $ Call (ident "a") [(ident "b"), (ident "c")]) `shouldBe` "a(b, c)"
      it "transpiles casts" $ do
        showexpr (te (e $ Cast (ident "a") (ParameterizedTypePath (([], s_pack ""), []))) (BasicType $ TypeVoid)) `shouldBe` "(void) a"
        showexpr (te (e $ Cast (ident "abc") (ParameterizedTypePath (([], s_pack ""), []))) (BasicType $ TypeInt 8)) `shouldBe` "(signed char) abc"
      {-it "transpiles vector literals" $ do
        showexpr (e $ VectorLiteral [(ident "b"), (ident "c")]) `shouldBe` "[b, c]"-}

      -- TODO: vector literal
