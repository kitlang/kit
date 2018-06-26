module Kit.Parser.LexerSpec where

  import Test.Hspec
  import Test.QuickCheck
  import Kit.Ast
  import Kit.Str
  import Kit.Parser

  lx s = map token_type $ scanTokens s
  lx2 s = scanTokens s

  spec :: Spec
  spec = do
    describe "Lexer" $ do
      it "lexes identifiers" $ do
        lx2 "apple Banana $macro_var ${macro_var2}" `shouldBe` [
            (LowerIdentifier "apple", sp 1 1 1 5),
            (UpperIdentifier "Banana", sp 1 7 1 12),
            (MacroIdentifier "macro_var", sp 1 14 1 23),
            (MacroIdentifier "macro_var2", sp 1 25 1 37)
          ]
      it "lexes parens" $ do
        lx2 "()" `shouldBe` [(ParenOpen, sp 1 1 1 1), (ParenClose, sp 1 2 1 2)]
      it "lexes keywords" $ do
        lx "abstract inline for in atom public rule rules" `shouldBe` [KeywordAbstract, KeywordInline, KeywordFor, KeywordIn, KeywordAtom, KeywordPublic, KeywordRule, KeywordRules]
      it "skips whitespace" $ do
        lx2 "  \t  \n  \r\t \na \n\n\n\t\t\t\r\n\r\n \r \n" `shouldBe` [(LowerIdentifier "a", sp 3 1 3 1)]
      it "skips comments" $ do
        lx2 "  // this is a comment\na" `shouldBe` [(LowerIdentifier "a", sp 2 1 2 1)]
      it "skips multiline comments" $ do
        lx2 "  /* this is a comment\n that spans multiple lines */a" `shouldBe` [(LowerIdentifier "a", sp 2 30 2 30)]
      it "lexes doc comments" $ do
        lx2 "  /** this is a comment\n*/a" `shouldBe` [(DocComment ("this is a comment\n"), sp 1 3 2 2), (LowerIdentifier "a", sp 2 3 2 3)]
      it "lexes operators" $ do
        lx "a++ += = 2 + 3 4;" `shouldBe` [LowerIdentifier "a", Op Inc, Op $ AssignOp Add, Op Assign, LiteralInt "2", Op Add, LiteralInt "3", LiteralInt "4", Semicolon]
      it "lexes custom operators" $ do
        lx "+ +-* -" `shouldBe` [Op Add, Op $ Custom ("+-*"), Op Sub]
      it "lexes int literals" $ do
        lx "1 234 0 0x123 0b101 0o0701 -1" `shouldBe` [LiteralInt "1", LiteralInt "234", LiteralInt "0", LiteralInt "0x123", LiteralInt "0b101", LiteralInt "0o0701", LiteralInt ("-1")]
      it "lexes string literals" $ do
        lx "'abc' \"def\" \"\"\"ghi\njkl\"\"\"" `shouldBe` [LiteralString "abc", LiteralString "def", LiteralString ("ghi\njkl")]
      it "lexes float literals" $ do
        lx "0.1 0.10000 -1.0" `shouldBe` [LiteralFloat ("0.1"), LiteralFloat ("0.10000"), LiteralFloat ("-1.0")]
      it "lexes bool literals" $ do
        lx "true false" `shouldBe` [LiteralBool True, LiteralBool False]
      it "lexes keywords" $ do
        lx "public in inline this token" `shouldBe` [KeywordPublic, KeywordIn, KeywordInline, KeywordThis, KeywordToken]
      it "lexes function call block" $ do
        lx "  {\nf.ghi(abc)\n}" `shouldBe` [CurlyBraceOpen, LowerIdentifier "f", Dot, LowerIdentifier "ghi", ParenOpen, LowerIdentifier "abc", ParenClose, CurlyBraceClose]
      it "lexes metadata" $ do
        lx "#[abc][]" `shouldBe` [MetaOpen, LowerIdentifier "abc", SquareBraceClose, SquareBraceOpen, SquareBraceClose]
