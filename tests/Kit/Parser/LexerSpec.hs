module Kit.Parser.LexerSpec where

  import Test.Hspec
  import Test.QuickCheck
  import Kit.Str
  import Kit.Ast.Operator
  import Kit.Parser.Span
  import Kit.Parser.Lexer
  import Kit.Parser.Token

  lx s = map token_type $ scanTokens (s_pack s)
  lx2 s = scanTokens (s_pack s)

  spec :: Spec
  spec = do
    describe "Lexer" $ do
      it "lexes identifiers" $ do
        lx2 "apple Banana $macro_var" `shouldBe` [
            (LowerIdentifier $ s_pack "apple", sp 1 1 1 5),
            (UpperIdentifier $ s_pack "Banana", sp 1 7 1 12),
            (MacroIdentifier $ s_pack "macro_var", sp 1 14 1 23)
          ]
      it "lexes parens" $ do
        lx2 "()" `shouldBe` [(ParenOpen, sp 1 1 1 1), (ParenClose, sp 1 2 1 2)]
      it "lexes keywords" $ do
        lx "abstract inline for in atom public rule rules" `shouldBe` [KeywordAbstract, KeywordInline, KeywordFor, KeywordIn, KeywordAtom, KeywordPublic, KeywordRule, KeywordRules]
      it "skips whitespace" $ do
        lx2 "  \t  \n  \r\t \na \n\n\n\t\t\t\r\n\r\n \r \n" `shouldBe` [(LowerIdentifier $ s_pack "a", sp 3 1 3 1)]
      it "skips comments" $ do
        lx2 "  // this is a comment\na" `shouldBe` [(LowerIdentifier (s_pack "a"), sp 2 1 2 1)]
      it "skips multiline comments" $ do
        lx2 "  /* this is a comment\n that spans multiple lines */a" `shouldBe` [(LowerIdentifier (s_pack "a"), sp 2 30 2 30)]
      it "lexes doc comments" $ do
        lx2 "  /** this is a comment\n*/a" `shouldBe` [(DocComment (s_pack "this is a comment\n"), sp 1 3 2 2), (LowerIdentifier (s_pack "a"), sp 2 3 2 3)]
      it "lexes operators" $ do
        lx "a++ += = 2 + 3 4;" `shouldBe` [LowerIdentifier (s_pack "a"), Op Inc, Op $ AssignOp Add, Op Assign, LiteralInt (s_pack "2"), Op Add, LiteralInt (s_pack "3"), LiteralInt (s_pack "4"), Semicolon]
      it "lexes custom operators" $ do
        lx "+ +-* -" `shouldBe` [Op Add, Op $ Custom (s_pack "+-*"), Op Sub]
      it "lexes int literals" $ do
        lx "1 234 0 0x123 0b101 0o0701 -1" `shouldBe` [LiteralInt (s_pack "1"), LiteralInt (s_pack "234"), LiteralInt (s_pack "0"), LiteralInt (s_pack "0x123"), LiteralInt (s_pack "0b101"), LiteralInt (s_pack "0o0701"), LiteralInt (s_pack "-1")]
      it "lexes string literals" $ do
        lx "'abc' \"def\" \"\"\"ghi\njkl\"\"\"" `shouldBe` [LiteralString (s_pack "abc"), LiteralString (s_pack "def"), LiteralString (s_pack "ghi\njkl")]
      it "lexes float literals" $ do
        lx "0.1 0.10000 -1.0" `shouldBe` [LiteralFloat (s_pack "0.1"), LiteralFloat (s_pack "0.10000"), LiteralFloat (s_pack "-1.0")]
      it "lexes bool literals" $ do
        lx "true false" `shouldBe` [LiteralBool True, LiteralBool False]
      it "lexes keywords" $ do
        lx "public in inline this token" `shouldBe` [KeywordPublic, KeywordIn, KeywordInline, KeywordThis, KeywordToken]
      it "lexes function call block" $ do
        lx "  {\nf.ghi(abc)\n}" `shouldBe` [CurlyBraceOpen, LowerIdentifier (s_pack "f"), Dot, LowerIdentifier (s_pack("ghi")), ParenOpen, LowerIdentifier (s_pack "abc"), ParenClose, CurlyBraceClose]
      it "lexes metadata" $ do
        lx "#[abc][]" `shouldBe` [MetaOpen, LowerIdentifier $ s_pack "abc", SquareBraceClose, SquareBraceOpen, SquareBraceClose]
