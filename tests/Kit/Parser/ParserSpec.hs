module Kit.Parser.ParserSpec where

  import qualified Data.ByteString.Lazy.Char8 as B
  import Test.Hspec
  import Test.QuickCheck
  import Kit.Ast.Expr
  import Kit.Ast.Modifier
  import Kit.Ast.Operator
  import Kit.Ast.Type
  import Kit.Ast.Value
  import Kit.Parser.Lexer
  import qualified Kit.Parser.Parser as P

  parse s = P.parseTokens (scanTokens (B.pack s))
  parseExpr s = P.parseExpr (scanTokens (B.pack s))
  parseStmt s = P.parseStatement (scanTokens (B.pack s))

  spec :: Spec
  spec = do
    describe "Parse expressions" $ do
      it "parses identifiers" $ do
        parseExpr "apple" `shouldBe` (e $ Identifier $ B.pack "apple")
        parseExpr "this" `shouldBe` (e $ This)
        parseExpr "Self" `shouldBe` (e $ Self)

      it "parses type annotations" $ do
        parseExpr "x : T" `shouldBe` (e $ TypeAnnotation (e $ Identifier $ B.pack "x") (ParameterizedTypePath (([],B.pack "T"),[])))

      it "parses value literals" $ do
        parseExpr "1" `shouldBe` (e $ Literal $ IntValue $ B.pack "1")

      it "parses binops" $ do
        parseExpr "a = 1 + 2.0 * 'abc def'" `shouldBe` (e $ Binop Assign (e $ Identifier $ B.pack "a") (e $ Binop Add (e $ Literal $ IntValue $ B.pack "1") (e $ Binop Mul (e $ Literal $ FloatValue $ B.pack "2.0") (e $ Literal $ StringValue $ B.pack "abc def"))))

      it "parses ternary" $ do
        parseExpr "if true then 1 else 2" `shouldBe` (e $ If (e $ Literal $ BoolValue True) (e $ Literal $ IntValue $ B.pack "1") (Just $ e $ Literal $ IntValue $ B.pack "2"))

      it "parses vectors" $ do
        parseExpr "[this, Self, true, false]" `shouldBe` (e $ VectorLiteral [e This, e Self, e $ Literal $ BoolValue True, e $ Literal $ BoolValue False])

    describe "Parse statements" $ do
      it "parses blocks" $ do
        parseStmt "{this; Self; break;}" `shouldBe` (e $ Block [e This, e Self, e Break])

      it "parses functions" $ do
        parseStmt "/**test*/ #[meta] inline function abc[A, B: Int, C: (ToString, ToInt)](a: A, b: B = 2, c: C): Something { print(a); print(b); print(c); }" `shouldBe` (e $
          FunctionDeclaration $ FunctionDefinition {
            function_name = B.pack "abc",
            function_doc = Just $ B.pack "test",
            function_meta = [Metadata {meta_name = B.pack "meta", meta_args = []}],
            function_modifiers = [Inline],
            function_params = [
              TypeParam {
                t = (([],B.pack "A"),[]),
                constraints = []
              },
              TypeParam {
                t = (([],B.pack "B"),[]),
                constraints = [ParameterizedTypePath (([],B.pack "Int"),[])]
              },
              TypeParam {
                t = (([],B.pack "C"),[]),
                constraints = [ParameterizedTypePath (([],B.pack "ToString"),[]),ParameterizedTypePath (([],B.pack "ToInt"),[])]
              }
            ],
            function_args = [
              ArgSpec {
                arg_name = B.pack "a",
                arg_type = Just $ ParameterizedTypePath (([],B.pack "A"),[]),
                arg_default = Nothing
              },
              ArgSpec {
                arg_name = B.pack "b",
                arg_type = Just $ ParameterizedTypePath (([],B.pack "B"),[]),
                arg_default = Just $ e $ Literal $ IntValue $ B.pack "2"
              },
              ArgSpec {
                arg_name = B.pack "c",
                arg_type = Just $ ParameterizedTypePath (([],B.pack "C"),[]),
                arg_default = Nothing
              }
            ],
            function_type = Just $ ParameterizedTypePath (([],B.pack "Something"),[]),
            function_body = Just $ e $ Block [
              e $ Call (e $ Identifier $ B.pack "print") [e $ Identifier $ B.pack "a"],
              e $ Call (e $ Identifier $ B.pack "print") [e $ Identifier $ B.pack "b"],
              e $ Call (e $ Identifier $ B.pack "print") [e $ Identifier $ B.pack "c"]
            ]
          })

    describe "Parse toplevel statements" $ do
      it "parses imports" $ do
        parse "import a;" `shouldBe` [e $ Import [B.pack "a"]]
        parse "import a.b.c;" `shouldBe` [e $ Import [B.pack "a", B.pack "b", B.pack "c"]]

      it "parses atoms" $ do
        parse "atom MyAtom;" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = B.pack "MyAtom",
          structure_type = Atom,
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = []
        }]
        parse "public atom MyAtom;" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = B.pack "MyAtom",
          structure_type = Atom,
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [Public],
          structure_rules = []
        }]
        parse "/** Doc*/ atom MyAtom;" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = B.pack "MyAtom",
          structure_type = Atom,
          structure_doc = Just $ B.pack "Doc",
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = []
        }]
        parse "/** Doc*/ #[meta] public atom MyAtom;" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = B.pack "MyAtom",
          structure_type = Atom,
          structure_doc = Just $ B.pack "Doc",
          structure_meta = [Metadata {meta_name = B.pack "meta", meta_args = []}],
          structure_modifiers = [Public],
          structure_rules = []
        }]

      it "parses enums" $ do
        parse "enum MyEnum {\n\
              \    Apple;\n\
              \    Banana(i: Int);\n\
              \    /**Abc*/ Strawberry;\n\
              \}" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = B.pack "MyEnum",
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = [],
          structure_type = Enum {
            enum_params = [],
            enum_variants = [
              EnumVariant {
                variant_name = B.pack "Apple",
                variant_doc = Nothing,
                variant_args = [],
                variant_meta = [],
                variant_modifiers = []
              },
              EnumVariant {
                variant_name = B.pack "Banana",
                variant_doc = Nothing,
                variant_args = [ArgSpec {arg_name = B.pack "i", arg_type = Just (ParameterizedTypePath (([],B.pack "Int"),[])), arg_default = Nothing}],
                variant_meta = [],
                variant_modifiers = []
              },
              EnumVariant {
                variant_name = B.pack "Strawberry",
                variant_doc = Just $ B.pack "Abc",
                variant_args = [],
                variant_meta = [],
                variant_modifiers = []
              }
            ]
          }
        }]

      it "parses structs" $ do
        parse "struct MyStruct {\n\
              \    var abc;\n\
              \    public var def;\n\
              \    /** test */ #[meta] var ghi: Int = 1;\n\
              \}" `shouldBe` [e $ TypeDeclaration $ Structure {
          structure_name = B.pack "MyStruct",
          structure_doc = Nothing,
          structure_meta = [],
          structure_modifiers = [],
          structure_rules = [],
          structure_type = Struct {
            struct_params = [],
            struct_fields = [
              VarDefinition {
                var_name = B.pack "abc",
                var_doc = Nothing,
                var_meta = [],
                var_modifiers = [],
                var_type = Nothing,
                var_default = Nothing
              },
              VarDefinition {
                var_name = B.pack "def",
                var_doc = Nothing,
                var_meta = [],
                var_modifiers = [Public],
                var_type = Nothing,
                var_default = Nothing
              },
              VarDefinition {
                var_name = B.pack "ghi",
                var_doc = Just $ B.pack "test ",
                var_meta = [Metadata {meta_name = B.pack "meta", meta_args = []}],
                var_modifiers = [],
                var_type = Just (ParameterizedTypePath (([], B.pack "Int"),[])),
                var_default = Just $ e $ Literal $ IntValue $ B.pack "1"
              }
            ]
          }
        }]
