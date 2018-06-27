module Kit.Parser.ParserSpec where

  import Kit.Str
  import Test.Hspec
  import Test.QuickCheck
  import Kit.Ast
  import Kit.Parser

  testParse s = unwrapParsed $ parseTokens (scanTokens Nothing s)
  testParseExpr s = unwrapParsed $ parseExpr (scanTokens Nothing s)
  testParseStmt s = unwrapParsed $ parseStatement (scanTokens Nothing s)
  unwrapParsed x = case x of
                     ParseResult r -> r
                     Err e -> error $ show e

  spec :: Spec
  spec = do
    describe "Parse expressions" $ do
      it "parses identifiers" $ do
        testParseExpr "apple" `shouldBe` (pe (sp 1 1 1 5) $ Lvalue $ Var "apple")
        testParseExpr "this" `shouldBe` (pe (sp 1 1 1 4) $ This)
        testParseExpr "Self" `shouldBe` (pe (sp 1 1 1 4) $ Self)

      it "parses type annotations" $ do
        testParseExpr "x : T" `shouldBe` (pe (sp 1 1 1 5) $ TypeAnnotation (pe (sp 1 1 1 1) $ Lvalue $ Var "x") (ParameterizedTypePath (([], "T"),[])))

      it "parses value literals" $ do
        testParseExpr "1" `shouldBe` (pe (sp 1 1 1 1) $ Literal $ IntValue "1")

      it "parses binops" $ do
        testParseExpr "a = 1 + 2.0 * 'abc def'" `shouldBe` (pe (sp 1 1 1 23) $ Binop Assign (e $ Lvalue $ Var "a") (e $ Binop Add (e $ Literal $ IntValue "1") (e $ Binop Mul (e $ Literal $ FloatValue "2.0") (e $ Literal $ StringValue "abc def"))))

      it "parses ternary" $ do
        testParseExpr "if true then 1 else 2" `shouldBe` (e $ If (e $ Literal $ BoolValue True) (e $ Literal $ IntValue "1") (Just $ e $ Literal $ IntValue "2"))

      it "parses vectors" $ do
        testParseExpr "[this, Self, true, false]" `shouldBe` (e $ VectorLiteral [e This, e Self, e $ Literal $ BoolValue True, e $ Literal $ BoolValue False])

      it "parses tokens" $ do
        testParseExpr "token |" `shouldBe` (e $ TokenExpr [Op BitOr])

    describe "Parse statements" $ do
      it "parses blocks" $ do
        testParseStmt "{this; Self;\n break;}" `shouldBe` (pe (sp 1 1 2 8) $ Block [pe (sp 1 2 1 6) This, pe (sp 1 8 1 12) Self, pe (sp 2 2 2 7) Break])

      it "parses continue" $ do
        testParseStmt "continue;" `shouldBe` (pe (sp 1 1 1 9) $ Continue)

      it "parses break" $ do
        testParseStmt "break;" `shouldBe` (pe (sp 1 1 1 6) $ Break)

      it "parses delete statements" $ do
        testParseStmt "delete a;" `shouldBe` (pe (sp 1 1 1 9) $ Delete (e $ Lvalue $ Var "a"))

      it "parses token blocks" $ do
        testParseStmt "tokens { }" `shouldBe` (e $ TokenExpr [])
        testParseStmt "tokens { a }" `shouldBe` (e $ TokenExpr [LowerIdentifier "a"])
        testParseStmt "tokens { a {} this, }" `shouldBe` (e $ TokenExpr [LowerIdentifier "a", CurlyBraceOpen, CurlyBraceClose, KeywordThis, Comma])

      it "parses functions" $ do
        testParse "/**test*/ #[meta] inline function abc[A, B: Int, C: (ToString, ToInt)](a: A, b: B = 2, c: C): Something { print(a); print(b); print(c); }" `shouldBe` [(pe (sp 1 11 1 137) $
          FunctionDeclaration $ FunctionDefinition {
            function_name = "abc",
            function_doc = Just "test",
            function_meta = [Metadata {meta_name = "meta", meta_args = []}],
            function_modifiers = [Inline],
            function_params = [
              TypeParam {
                t = (([], "A"),[]),
                constraints = []
              },
              TypeParam {
                t = (([], "B"),[]),
                constraints = [ParameterizedTypePath (([], "Int"),[])]
              },
              TypeParam {
                t = (([], "C"),[]),
                constraints = [ParameterizedTypePath (([], "ToString"),[]),ParameterizedTypePath (([], "ToInt"),[])]
              }
            ],
            function_args = [
              ArgSpec {
                arg_name = "a",
                arg_type = Just $ ParameterizedTypePath (([], "A"),[]),
                arg_default = Nothing
              },
              ArgSpec {
                arg_name = "b",
                arg_type = Just $ ParameterizedTypePath (([], "B"),[]),
                arg_default = Just $ e $ Literal $ IntValue "2"
              },
              ArgSpec {
                arg_name = "c",
                arg_type = Just $ ParameterizedTypePath (([], "C"),[]),
                arg_default = Nothing
              }
            ],
            function_type = Just $ ParameterizedTypePath (([], "Something"),[]),
            function_body = Just $ e $ Block [
              e $ Call (e $ Lvalue $ Var "print") [e $ Lvalue $ Var "a"],
              e $ Call (e $ Lvalue $ Var "print") [e $ Lvalue $ Var "b"],
              e $ Call (e $ Lvalue $ Var "print") [e $ Lvalue $ Var "c"]
            ]
          })]

    describe "Parse toplevel statements" $ do
      it "parses imports" $ do
        testParse "import a;" `shouldBe` [e $ Import ["a"]]
        testParse "import a.b.c; import d;" `shouldBe` [e $ Import ["a", "b", "c"], e $ Import ["d"]]


      it "parses typedefs" $ do
        testParse "typedef MyType = OtherType;" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = "MyType",
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = [],
          structure_type = Typedef {
            typedef_params = [],
            typedef_definition = ParameterizedTypePath (([], "OtherType"), [])
          }
        }]
        testParse "typedef MyType[A] = OtherType[B];" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = "MyType",
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = [],
          structure_type = Typedef {
            typedef_params = [TypeParam {t = (([], "A"),[]), constraints = []}],
            typedef_definition = ParameterizedTypePath (([], "OtherType"), [TypeParam {t = (([], "B"),[]), constraints = []}])
          }
        }]

      it "parses atoms" $ do
        testParse "atom MyAtom;" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = "MyAtom",
          structure_type = Atom,
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = []
        }]
        testParse "public atom MyAtom;" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = "MyAtom",
          structure_type = Atom,
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [Public],
          structure_rules = []
        }]
        testParse "/** Doc*/ atom MyAtom;" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = "MyAtom",
          structure_type = Atom,
          structure_doc = Just "Doc",
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = []
        }]
        testParse "/** Doc*/ #[meta] public atom MyAtom;" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = "MyAtom",
          structure_type = Atom,
          structure_doc = Just "Doc",
          structure_meta = [Metadata {meta_name = "meta", meta_args = []}],
          structure_modifiers = [Public],
          structure_rules = []
        }]

      it "parses enums" $ do
        testParse "enum MyEnum: Float {\n\
              \    Apple;\n\
              \    Banana(i: Int);\n\
              \    /**Abc*/ Strawberry = 1;\n\
              \}" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = "MyEnum",
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = [],
          structure_type = Enum {
            enum_params = [],
            enum_underlying_type = Just (ParameterizedTypePath (([], "Float"),[])),
            enum_variants = [
              EnumVariant {
                variant_name = "Apple",
                variant_doc = Nothing,
                variant_args = [],
                variant_meta = [],
                variant_modifiers = [],
                variant_value = Nothing
              },
              EnumVariant {
                variant_name = "Banana",
                variant_doc = Nothing,
                variant_args = [ArgSpec {arg_name = "i", arg_type = Just (ParameterizedTypePath (([], "Int"),[])), arg_default = Nothing}],
                variant_meta = [],
                variant_modifiers = [],
                variant_value = Nothing
              },
              EnumVariant {
                variant_name = "Strawberry",
                variant_doc = Just "Abc",
                variant_args = [],
                variant_meta = [],
                variant_modifiers = [],
                variant_value = Just (e $ Literal $ IntValue "1")
              }
            ]
          }
        }]

      it "parses structs" $ do
        testParse "struct MyStruct {\n\
              \    var abc;\n\
              \    public var def;\n\
              \    /** test */ #[meta] var ghi: Int = 1;\n\
              \}" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = "MyStruct",
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = [],
          structure_type = Struct {
            struct_params = [],
            struct_fields = [
              VarDefinition {
                var_name = Var "abc",
                var_doc = Nothing,
                var_meta = [],
                var_modifiers = [],
                var_type = Nothing,
                var_default = Nothing
              },
              VarDefinition {
                var_name = Var "def",
                var_doc = Nothing,
                var_meta = [],
                var_modifiers = [Public],
                var_type = Nothing,
                var_default = Nothing
              },
              VarDefinition {
                var_name = Var "ghi",
                var_doc = Just "test ",
                var_meta = [Metadata {meta_name = "meta", meta_args = []}],
                var_modifiers = [],
                var_type = Just (ParameterizedTypePath (([], "Int"),[])),
                var_default = Just $ e $ Literal $ IntValue "1"
              }
            ]
          }
        }]

    describe "Parses expression lists" $ do
      it "parses multiple statements" $ do
        testParse "import a; import b;" `shouldBe` [e $ Import ["a"], e $ Import ["b"]]
