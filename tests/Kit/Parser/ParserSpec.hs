module Kit.Parser.ParserSpec where

  import Kit.Str
  import Test.Hspec
  import Test.QuickCheck
  import Kit.Ast.Expr
  import Kit.Ast.Modifier
  import Kit.Ast.Operator
  import Kit.Parser.Span
  import Kit.Ast.Type
  import Kit.Ast.Value
  import Kit.Parser.Lexer
  import Kit.Parser.Parser
  import Kit.Parser.Token

  testParse s = unwrapParsed $ parseTokens (scanTokens (s_pack s))
  testParseExpr s = unwrapParsed $ parseExpr (scanTokens (s_pack s))
  testParseStmt s = unwrapParsed $ parseStatement (scanTokens (s_pack s))
  unwrapParsed x = case x of
                     ParseResult r -> r
                     Err e -> error $ show e

  spec :: Spec
  spec = do
    describe "Parse expressions" $ do
      it "parses identifiers" $ do
        testParseExpr "apple" `shouldBe` (pe (sp 1 1 1 5) $ Lvalue $ Var $ s_pack "apple")
        testParseExpr "this" `shouldBe` (pe (sp 1 1 1 4) $ This)
        testParseExpr "Self" `shouldBe` (pe (sp 1 1 1 4) $ Self)

      it "parses type annotations" $ do
        testParseExpr "x : T" `shouldBe` (pe (sp 1 1 1 5) $ TypeAnnotation (pe (sp 1 1 1 1) $ Lvalue $ Var $ s_pack "x") (ParameterizedTypePath (([],s_pack "T"),[])))

      it "parses value literals" $ do
        testParseExpr "1" `shouldBe` (pe (sp 1 1 1 1) $ Literal $ IntValue $ s_pack "1")

      it "parses binops" $ do
        testParseExpr "a = 1 + 2.0 * 'abc def'" `shouldBe` (pe (sp 1 1 1 23) $ Binop Assign (e $ Lvalue $ Var $ s_pack "a") (e $ Binop Add (e $ Literal $ IntValue $ s_pack "1") (e $ Binop Mul (e $ Literal $ FloatValue $ s_pack "2.0") (e $ Literal $ StringValue $ s_pack "abc def"))))

      it "parses ternary" $ do
        testParseExpr "if true then 1 else 2" `shouldBe` (e $ If (e $ Literal $ BoolValue True) (e $ Literal $ IntValue $ s_pack "1") (Just $ e $ Literal $ IntValue $ s_pack "2"))

      it "parses vectors" $ do
        testParseExpr "[this, Self, true, false]" `shouldBe` (e $ VectorLiteral [e This, e Self, e $ Literal $ BoolValue True, e $ Literal $ BoolValue False])

      it "parses tokens" $ do
        testParseExpr "token |" `shouldBe` (e $ TokenExpr [Op BitOr])

    describe "Parse statements" $ do
      it "parses blocks" $ do
        testParseStmt "{this; Self;\n break;}" `shouldBe` (pe (sp 1 1 2 8) $ Block [pe (sp 1 2 1 6) This, pe (sp 1 8 1 12) Self, pe (sp 2 2 2 7) Break])

      it "parses token blocks" $ do
        testParseStmt "tokens { }" `shouldBe` (e $ TokenExpr [])
        testParseStmt "tokens { a }" `shouldBe` (e $ TokenExpr [LowerIdentifier $ s_pack "a"])
        testParseStmt "tokens { a {} this, }" `shouldBe` (e $ TokenExpr [LowerIdentifier $ s_pack "a", CurlyBraceOpen, CurlyBraceClose, KeywordThis, Comma])

      it "parses functions" $ do
        testParseStmt "/**test*/ #[meta] inline function abc[A, B: Int, C: (ToString, ToInt)](a: A, b: B = 2, c: C) => Something { print(a); print(b); print(c); }" `shouldBe` (pe (sp 1 11 1 139) $
          FunctionDeclaration $ FunctionDefinition {
            function_name = s_pack "abc",
            function_doc = Just $ s_pack "test",
            function_meta = [Metadata {meta_name = s_pack "meta", meta_args = []}],
            function_modifiers = [Inline],
            function_params = [
              TypeParam {
                t = (([],s_pack "A"),[]),
                constraints = []
              },
              TypeParam {
                t = (([],s_pack "B"),[]),
                constraints = [ParameterizedTypePath (([],s_pack "Int"),[])]
              },
              TypeParam {
                t = (([],s_pack "C"),[]),
                constraints = [ParameterizedTypePath (([],s_pack "ToString"),[]),ParameterizedTypePath (([],s_pack "ToInt"),[])]
              }
            ],
            function_args = [
              ArgSpec {
                arg_name = s_pack "a",
                arg_type = Just $ ParameterizedTypePath (([],s_pack "A"),[]),
                arg_default = Nothing
              },
              ArgSpec {
                arg_name = s_pack "b",
                arg_type = Just $ ParameterizedTypePath (([],s_pack "B"),[]),
                arg_default = Just $ e $ Literal $ IntValue $ s_pack "2"
              },
              ArgSpec {
                arg_name = s_pack "c",
                arg_type = Just $ ParameterizedTypePath (([],s_pack "C"),[]),
                arg_default = Nothing
              }
            ],
            function_type = Just $ ParameterizedTypePath (([],s_pack "Something"),[]),
            function_body = Just $ e $ Block [
              e $ Call (e $ Lvalue $ Var $ s_pack "print") [e $ Lvalue $ Var $ s_pack "a"],
              e $ Call (e $ Lvalue $ Var $ s_pack "print") [e $ Lvalue $ Var $ s_pack "b"],
              e $ Call (e $ Lvalue $ Var $ s_pack "print") [e $ Lvalue $ Var $ s_pack "c"]
            ]
          })

    describe "Parse toplevel statements" $ do
      it "parses imports" $ do
        testParse "import a;" `shouldBe` [e $ Import [s_pack "a"]]
        testParse "import a.b.c; import d;" `shouldBe` [e $ Import [s_pack "a", s_pack "b", s_pack "c"], e $ Import [s_pack "d"]]

      it "parses atoms" $ do
        testParse "atom MyAtom;" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = s_pack "MyAtom",
          structure_type = Atom,
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = []
        }]
        testParse "public atom MyAtom;" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = s_pack "MyAtom",
          structure_type = Atom,
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [Public],
          structure_rules = []
        }]
        testParse "/** Doc*/ atom MyAtom;" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = s_pack "MyAtom",
          structure_type = Atom,
          structure_doc = Just $ s_pack "Doc",
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = []
        }]
        testParse "/** Doc*/ #[meta] public atom MyAtom;" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = s_pack "MyAtom",
          structure_type = Atom,
          structure_doc = Just $ s_pack "Doc",
          structure_meta = [Metadata {meta_name = s_pack "meta", meta_args = []}],
          structure_modifiers = [Public],
          structure_rules = []
        }]

      it "parses enums" $ do
        testParse "enum MyEnum {\n\
              \    Apple;\n\
              \    Banana(i: Int);\n\
              \    /**Abc*/ Strawberry;\n\
              \}" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = s_pack "MyEnum",
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = [],
          structure_type = Enum {
            enum_params = [],
            enum_variants = [
              EnumVariant {
                variant_name = s_pack "Apple",
                variant_doc = Nothing,
                variant_args = [],
                variant_meta = [],
                variant_modifiers = []
              },
              EnumVariant {
                variant_name = s_pack "Banana",
                variant_doc = Nothing,
                variant_args = [ArgSpec {arg_name = s_pack "i", arg_type = Just (ParameterizedTypePath (([],s_pack "Int"),[])), arg_default = Nothing}],
                variant_meta = [],
                variant_modifiers = []
              },
              EnumVariant {
                variant_name = s_pack "Strawberry",
                variant_doc = Just $ s_pack "Abc",
                variant_args = [],
                variant_meta = [],
                variant_modifiers = []
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
          structure_name = s_pack "MyStruct",
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = [],
          structure_type = Struct {
            struct_params = [],
            struct_fields = [
              VarDefinition {
                var_name = Var $ s_pack "abc",
                var_doc = Nothing,
                var_meta = [],
                var_modifiers = [],
                var_type = Nothing,
                var_default = Nothing
              },
              VarDefinition {
                var_name = Var $ s_pack "def",
                var_doc = Nothing,
                var_meta = [],
                var_modifiers = [Public],
                var_type = Nothing,
                var_default = Nothing
              },
              VarDefinition {
                var_name = Var $ s_pack "ghi",
                var_doc = Just $ s_pack "test ",
                var_meta = [Metadata {meta_name = s_pack "meta", meta_args = []}],
                var_modifiers = [],
                var_type = Just (ParameterizedTypePath (([], s_pack "Int"),[])),
                var_default = Just $ e $ Literal $ IntValue $ s_pack "1"
              }
            ]
          }
        }]
