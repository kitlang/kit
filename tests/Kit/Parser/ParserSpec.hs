module Kit.Parser.ParserSpec where

import Kit.Str
import Test.Hspec
import Test.QuickCheck
import Kit.Ast
import Kit.Parser

testParse s = unwrapParsed $ parseTokens (scanTokens "" s)
testParseExpr s = unwrapParsed $ parseExpr (scanTokens "" s)
testParseTopLevel s = unwrapParsed $ parseTopLevelExpr (scanTokens "" s)
unwrapParsed x = case x of
  ParseResult r -> r
  Err         e -> error $ show e

spec :: Spec
spec = parallel $ do
  describe "Parse expressions" $ do
    it "parses identifiers" $ do
      testParseExpr "apple"
        `shouldBe` (pe (sp "" 1 1 1 5) $ Identifier (Var "apple") [])
      testParseExpr "this" `shouldBe` (pe (sp "" 1 1 1 4) $ This)
      testParseExpr "Self" `shouldBe` (pe (sp "" 1 1 1 4) $ Self)

    it "parses type annotations" $ do
      testParseExpr "x : T"
        `shouldBe` (pe (sp "" 1 1 1 5) $ TypeAnnotation
                     (pe (sp "" 1 1 1 1) $ Identifier (Var "x") [])
                     (Just $ TypeSpec (([], "T")) [] NoPos)
                   )

    it "parses value literals" $ do
      testParseExpr "1"
        `shouldBe` (pe (sp "" 1 1 1 1) $ Literal $ IntValue 1 Nothing)

    it "parses binops" $ do
      testParseExpr "a = 1 + 2.0 * 'abc def'"
        `shouldBe` (pe (sp "" 1 1 1 23) $ Binop
                     Assign
                     (e $ Identifier (Var "a") [])
                     (e $ Binop
                       Add
                       (e $ Literal $ IntValue 1 Nothing)
                       (e $ Binop Mul
                                  (e $ Literal $ FloatValue "2.0" Nothing)
                                  (e $ Literal $ StringValue "abc def")
                       )
                     )
                   )

    it "parses ternary" $ do
      testParseExpr "if true then 1 else 2"
        `shouldBe` (e $ If (e $ Literal $ BoolValue True)
                           (e $ Literal $ IntValue 1 Nothing)
                           (Just $ e $ Literal $ IntValue 2 Nothing)
                   )

    it "parses vectors" $ do
      testParseExpr "[this, Self, true, false]"
        `shouldBe` (e $ VectorLiteral
                     [ e This
                     , e Self
                     , e $ Literal $ BoolValue True
                     , e $ Literal $ BoolValue False
                     ]
                   )

    -- it "parses tokens" $ do
    --   testParseExpr "token |" `shouldBe` (e $ TokenExpr [Op BitOr])

    it "parses casts" $ do
      testParseExpr "this as A[B]"
        `shouldBe` (e $ Cast
                     (e This)
                     (Just $ TypeSpec ([], "A")
                                      [typeParamToSpec $ makeTypeParam "B"]
                                      NoPos
                     )
                   )

    it "parses inline structs" $ do
      testParseExpr "struct Abc {a: 1, b: true}"
        `shouldBe` (e $ StructInit
                     (Just $ TypeSpec ([], "Abc") [] (sp "" 1 8 1 10))
                     [ ("a", e $ Literal $ IntValue 1 Nothing)
                     , ("b", e $ Literal $ BoolValue True)
                     ]
                   )

    it "parses tuple type specs" $ do
      testParseExpr "__: (A, B, C)"
        `shouldBe` (pe (sp "" 1 1 1 13) $ TypeAnnotation
                     (pe (sp "" 1 1 1 2) $ Identifier (Var "__") [])
                     (Just
                       (TupleTypeSpec
                         [ TypeSpec ([], "A") [] (sp "" 1 5 1 5)
                         , TypeSpec ([], "B") [] (sp "" 1 8 1 8)
                         , TypeSpec ([], "C") [] (sp "" 1 11 1 11)
                         ]
                         (sp "" 1 4 1 13)
                       )
                     )
                   )

    it "parses function type specs with no args" $ do
      testParseExpr "__: function () -> A"
        `shouldBe` (e $ TypeAnnotation
                     (e $ Identifier (Var "__") [])
                     (Just
                       (FunctionTypeSpec (TypeSpec ([], "A") [] NoPos)
                                         []
                                         False
                                         NoPos
                       )
                     )
                   )

    it "parses function type specs with one arg" $ do
      testParseExpr "__: function (Int) -> A"
        `shouldBe` (e $ TypeAnnotation
                     (e $ Identifier (Var "__") [])
                     (Just
                       (FunctionTypeSpec (TypeSpec ([], "A") [] NoPos)
                                         [(TypeSpec ([], "Int") [] NoPos)]
                                         False
                                         NoPos
                       )
                     )
                   )

    it "parses function type specs with args" $ do
      testParseExpr "__: function (Int, Float) -> A"
        `shouldBe` (e $ TypeAnnotation
                     (e $ Identifier (Var "__") [])
                     (Just
                       (FunctionTypeSpec
                         (TypeSpec ([], "A") [] NoPos)
                         [ (TypeSpec ([], "Int") [] NoPos)
                         , (TypeSpec ([], "Float") [] NoPos)
                         ]
                         False
                         NoPos
                       )
                     )
                   )

    it "parses nested function type specs" $ do
      testParseExpr "__: function (function (A, B) -> C, Float) -> A"
        `shouldBe` (e $ TypeAnnotation
                     (e $ Identifier (Var "__") [])
                     (Just
                       (FunctionTypeSpec
                         (TypeSpec ([], "A") [] NoPos)
                         [ (FunctionTypeSpec
                             (TypeSpec ([], "C") [] NoPos)
                             [ (TypeSpec ([], "A") [] NoPos)
                             , (TypeSpec ([], "B") [] NoPos)
                             ]
                             False
                             NoPos
                           )
                         , (TypeSpec ([], "Float") [] NoPos)
                         ]
                         False
                         NoPos
                       )
                     )
                   )

    it "parses variadic function type specs" $ do
      testParseExpr "__: function (Int, ...) -> A"
        `shouldBe` (e $ TypeAnnotation
                     (e $ Identifier (Var "__") [])
                     (Just
                       (FunctionTypeSpec (TypeSpec ([], "A") [] NoPos)
                                         [(TypeSpec ([], "Int") [] NoPos)]
                                         True
                                         NoPos
                       )
                     )
                   )

  describe "Parse statements" $ do
    it "parses blocks" $ do
      testParseTopLevel "{this; Self;\n break;}"
        `shouldBe` (pe (sp "" 1 1 2 8) $ Block
                     [ pe (sp "" 1 2 1 6)  This
                     , pe (sp "" 1 8 1 12) Self
                     , pe (sp "" 2 2 2 7)  Break
                     ]
                   )

    it "parses continue" $ do
      testParseTopLevel "continue;" `shouldBe` (pe (sp "" 1 1 1 9) $ Continue)

    it "parses break" $ do
      testParseTopLevel "break;" `shouldBe` (pe (sp "" 1 1 1 6) $ Break)

    -- it "parses token blocks" $ do
    --   testParseTopLevel "tokens { }" `shouldBe` (e $ TokenExpr [])
    --   testParseTopLevel "tokens { a }"
    --     `shouldBe` (e $ TokenExpr [LowerIdentifier "a"])
    --   testParseTopLevel "tokens { a {} this, }"
    --     `shouldBe` (e $ TokenExpr
    --                  [ LowerIdentifier "a"
    --                  , CurlyBraceOpen
    --                  , CurlyBraceClose
    --                  , KeywordThis
    --                  , Comma
    --                  ]
    --                )

    it "parses functions" $ do
      testParse
          "/**test*/ #[meta] inline function abc[A, B: Int, C :: ToString | ToInt](a: A, b: B = 2, c: C, ...): Something { print(a); print(b); print(c); }"
        `shouldBe` [ ( ps (sp "" 1 26 1 37)
                     $ FunctionDeclaration
                     $ (newFunctionDefinition :: FunctionDefinition
                           Expr
                           (Maybe TypeSpec)
                       )
                         { functionName      = "abc"
                         , functionDoc       = Just "test"
                         , functionMeta      = [ Metadata
                                                   { metaName = "meta"
                                                   , metaArgs = []
                                                   }
                                               ]
                         , functionModifiers = [Inline]
                         , functionParams    = [ makeTypeParam "A"
                                               , (makeTypeParam "B")
                                                 { constraints = [ makeTypeSpec
                                                                     "Int"
                                                                 ]
                                                 }
                                               , (makeTypeParam "C")
                                                 { constraints = [ makeTypeSpec
                                                                   "ToString"
                                                                 , makeTypeSpec
                                                                   "ToInt"
                                                                 ]
                                                 }
                                               ]
                         , functionArgs      = [ newArgSpec
                                                 { argName = "a"
                                                 , argType = Just
                                                   $ makeTypeSpec "A"
                                                 }
                                               , newArgSpec
                                                 { argName    = "b"
                                                 , argType    = Just
                                                   $ makeTypeSpec "B"
                                                 , argDefault = Just
                                                   $ e
                                                   $ Literal
                                                   $ IntValue 2 Nothing
                                                 }
                                               , newArgSpec
                                                 { argName = "c"
                                                 , argType = Just
                                                   $ makeTypeSpec "C"
                                                 }
                                               ]
                         , functionType      = Just $ makeTypeSpec "Something"
                         , functionBody      = Just $ e $ Block
                           [ e $ Call (e $ Identifier (Var "print") [])
                                      [e $ Identifier (Var "a") []]
                           , e $ Call (e $ Identifier (Var "print") [])
                                      [e $ Identifier (Var "b") []]
                           , e $ Call (e $ Identifier (Var "print") [])
                                      [e $ Identifier (Var "c") []]
                           ]
                         , functionVarargs   = True
                         }
                     )
                   ]

  describe "Parse toplevel statements" $ do
    it "parses imports" $ do
      testParse "import a;" `shouldBe` [makeStmt $ Import ["a"]]
      testParse "import a.b.c; import d;"
        `shouldBe` [makeStmt $ Import ["a", "b", "c"], makeStmt $ Import ["d"]]


    it "parses typedefs" $ do
      testParse "typedef MyType = OtherType;"
        `shouldBe` [makeStmt $ Typedef "MyType" $ makeTypeSpec "OtherType"]
      {-testParse "typedef MyType[A] = OtherType[B];" `shouldBe` [makeStmt $ TypeDeclaration $ TypeDefinition {
        typeName = "MyType",
        typeDoc = Nothing,
        typeMeta = [],
        typeModifiers = [],
        typeRules = [],
        typeParams = [TypeParam {paramType = makeTypeSpec "A", constraints = []}],
        typeSubtype = Typedef {
          typedef_definition = TypeSpec ([], "OtherType") [TypeParam {paramType = makeTypeSpec "B", constraints = []}] NoPos
        }
      }]-}

    it "parses atoms" $ do
      testParse "atom MyAtom;"
        `shouldBe` [ makeStmt $ TypeDeclaration $ (newTypeDefinition)
                       { typeName      = "MyAtom"
                       , typeSubtype   = Atom
                       , typeDoc       = Nothing
                       , typeMeta      = []
                       , typeModifiers = []
                       , typeRules     = []
                       , typeParams    = []
                       }
                   ]
      testParse "public atom MyAtom;"
        `shouldBe` [ makeStmt $ TypeDeclaration $ (newTypeDefinition)
                       { typeName      = "MyAtom"
                       , typeSubtype   = Atom
                       , typeDoc       = Nothing
                       , typeMeta      = []
                       , typeModifiers = [Public]
                       , typeRules     = []
                       , typeParams    = []
                       }
                   ]
      testParse "/** Doc*/ atom MyAtom;"
        `shouldBe` [ makeStmt $ TypeDeclaration $ (newTypeDefinition)
                       { typeName      = "MyAtom"
                       , typeSubtype   = Atom
                       , typeDoc       = Just "Doc"
                       , typeMeta      = []
                       , typeModifiers = []
                       , typeRules     = []
                       , typeParams    = []
                       }
                   ]
      testParse "/** Doc*/ #[meta] public atom MyAtom;"
        `shouldBe` [ makeStmt $ TypeDeclaration $ (newTypeDefinition)
                       { typeName      = "MyAtom"
                       , typeSubtype   = Atom
                       , typeDoc       = Just "Doc"
                       , typeMeta      = [ Metadata
                                             { metaName = "meta"
                                             , metaArgs = []
                                             }
                                         ]
                       , typeModifiers = [Public]
                       , typeRules     = []
                       , typeParams    = []
                       }
                   ]

    it "parses enums" $ do
      testParse
          "enum MyEnum: Float {\n\
            \    Apple;\n\
            \    Banana(i: Int);\n\
            \    /**Abc*/ Strawberry = 1;\n\
            \}"
        `shouldBe` [ makeStmt $ TypeDeclaration $ (newTypeDefinition)
                       { typeName      = "MyEnum"
                       , typeDoc       = Nothing
                       , typeMeta      = []
                       , typeModifiers = []
                       , typeRules     = []
                       , typeParams    = []
                       , typeSubtype   = Enum
                         { enumUnderlyingType = Just (makeTypeSpec "Float")
                         , enumVariants       = [ newEnumVariant
                                                  { variantName      = "Apple"
                                                  , variantDoc       = Nothing
                                                  , variantArgs      = []
                                                  , variantMeta      = []
                                                  , variantModifiers = []
                                                  , variantValue     = Nothing
                                                  }
                                                , newEnumVariant
                                                  { variantName      = "Banana"
                                                  , variantDoc       = Nothing
                                                  , variantArgs = [ newArgSpec
                                                                      { argName = "i"
                                                                      , argType = Just
                                                                        (makeTypeSpec
                                                                          "Int"
                                                                        )
                                                                      }
                                                                  ]
                                                  , variantMeta      = []
                                                  , variantModifiers = []
                                                  , variantValue     = Nothing
                                                  }
                                                , newEnumVariant
                                                  { variantName = "Strawberry"
                                                  , variantDoc = Just "Abc"
                                                  , variantArgs = []
                                                  , variantMeta = []
                                                  , variantModifiers = []
                                                  , variantValue = Just
                                                    (e $ Literal $ IntValue 1 Nothing)
                                                  }
                                                ]
                         }
                       }
                   ]

    it "parses structs" $ do
      testParse
          "struct MyStruct {\n\
            \    var abc;\n\
            \    public var def;\n\
            \    /** test */ #[meta] var ghi: Int = 1;\n\
            \\n\
            \    static var ghi;\n\
            \    static function jkl() {}\n\
            \}"
        `shouldBe` [ makeStmt $ TypeDeclaration $ (newTypeDefinition)
                       { typeName          = "MyStruct"
                       , typeDoc           = Nothing
                       , typeMeta          = []
                       , typeModifiers     = []
                       , typeRules         = []
                       , typeParams        = []
                       , typeStaticFields  = [ newVarDefinition
                                                 { varName      = "ghi"
                                                 , varDoc       = Nothing
                                                 , varMeta      = []
                                                 , varModifiers = [Static]
                                                 , varType      = Nothing
                                                 , varDefault   = Nothing
                                                 , varNamespace = []
                                                 }
                                             ]
                       , typeStaticMethods = [ newFunctionDefinition
                                                 { functionName      = "jkl"
                                                 , functionDoc       = Nothing
                                                 , functionMeta      = []
                                                 , functionModifiers = [Static]
                                                 , functionParams    = []
                                                 , functionArgs      = []
                                                 , functionType      = Nothing
                                                 , functionBody      = Just
                                                   (e $ Block [])
                                                 , functionVarargs   = False
                                                 , functionNamespace = []
                                                 , functionThis      = Nothing
                                                 , functionSelf      = Nothing
                                                 }
                                             ]
                       , typeSubtype       = Struct
                         { structFields = [ (newVarDefinition :: VarDefinition
                                                Expr
                                                (Maybe TypeSpec)
                                            )
                                            { varName      = "abc"
                                            , varDoc       = Nothing
                                            , varMeta      = []
                                            , varModifiers = []
                                            , varType      = Nothing
                                            , varDefault   = Nothing
                                            }
                                          , (newVarDefinition :: VarDefinition
                                                Expr
                                                (Maybe TypeSpec)
                                            )
                                            { varName      = "def"
                                            , varDoc       = Nothing
                                            , varMeta      = []
                                            , varModifiers = [Public]
                                            , varType      = Nothing
                                            , varDefault   = Nothing
                                            }
                                          , (newVarDefinition :: VarDefinition
                                                Expr
                                                (Maybe TypeSpec)
                                            )
                                            { varName      = "ghi"
                                            , varDoc       = Just "test "
                                            , varMeta      = [ Metadata
                                                                 { metaName = "meta"
                                                                 , metaArgs = []
                                                                 }
                                                             ]
                                            , varModifiers = []
                                            , varType      = Just
                                              (makeTypeSpec "Int")
                                            , varDefault   = Just
                                              $ e
                                              $ Literal
                                              $ IntValue 1 Nothing
                                            }
                                          ]
                         }
                       }
                   ]

  describe "Parses expression lists" $ do
    it "parses multiple statements" $ do
      testParse "import a; import b; import c; import d;"
        `shouldBe` [ makeStmt $ Import ["a"]
                   , makeStmt $ Import ["b"]
                   , makeStmt $ Import ["c"]
                   , makeStmt $ Import ["d"]
                   ]
