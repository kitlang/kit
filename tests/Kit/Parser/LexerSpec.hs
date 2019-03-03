module Kit.Parser.LexerSpec where

import Test.Hspec
import Test.QuickCheck
import Kit.Ast
import Kit.Str
import Kit.Parser

lx s = map token_type $ scanTokens (FileSpan "") s
lx2 s = scanTokens (FileSpan "") s

spec :: Spec
spec = parallel $ do
  describe "Lexer" $ do
    it "lexes identifiers" $ do
      lx2 "apple Banana $macro_var ${macro_var2} _ __ @true"
        `shouldBe` [ (LowerIdentifier "apple"     , sp "" 1 1 1 5)
                   , (UpperIdentifier "Banana"    , sp "" 1 7 1 12)
                   , (MacroIdentifier "macro_var" , sp "" 1 14 1 23)
                   , (MacroIdentifier "macro_var2", sp "" 1 25 1 37)
                   , (Underscore                  , sp "" 1 39 1 39)
                   , (LowerIdentifier "__"        , sp "" 1 41 1 42)
                   , (LowerIdentifier "true"      , sp "" 1 44 1 48)
                   ]
    it "lexes parens" $ do
      lx2 "()"
        `shouldBe` [(ParenOpen, sp "" 1 1 1 1), (ParenClose, sp "" 1 2 1 2)]
      lx2 "(...) ->"
        `shouldBe` [ (ParenOpen    , sp "" 1 1 1 1)
                   , (TripleDot    , sp "" 1 2 1 4)
                   , (ParenClose   , sp "" 1 5 1 5)
                   , (FunctionArrow, sp "" 1 7 1 8)
                   ]
    it "lexes keywords" $ do
      lx "abstract defined inline for in public rule rules undefined"
        `shouldBe` [ KeywordAbstract
                   , KeywordDefined
                   , KeywordInline
                   , KeywordFor
                   , KeywordIn
                   , KeywordPublic
                   , KeywordRule
                   , KeywordRules
                   , KeywordUndefined
                   ]
    it "skips whitespace" $ do
      lx2 "  \t  \n  \r\t \na \n\n\n\t\t\t\r\n\r\n \r \n"
        `shouldBe` [(LowerIdentifier "a", sp "" 3 1 3 1)]
    it "skips comments" $ do
      lx2 "  // this is a comment\na"
        `shouldBe` [(LowerIdentifier "a", sp "" 2 1 2 1)]
    it "skips multiline comments" $ do
      lx2 "  /* this is a comment\n that spans multiple lines */a"
        `shouldBe` [(LowerIdentifier "a", sp "" 2 30 2 30)]
    it "lexes operators" $ do
      lx "a++ += = 2 + 3 4;"
        `shouldBe` [ LowerIdentifier "a"
                   , Op Inc
                   , Op $ AssignOp Add
                   , Op Assign
                   , LiteralInt 2 Nothing
                   , Op Add
                   , LiteralInt 3 Nothing
                   , LiteralInt 4 Nothing
                   , Semicolon
                   ]
    it "lexes custom operators" $ do
      lx "+ +-* -" `shouldBe` [Op Add, Op $ Custom ("+-*"), Op Sub]
    it "lexes int literals" $ do
      lx "1 234 0" `shouldBe` map (\x -> LiteralInt x Nothing) [1, 234, 0]
    it "lexes char literals" $ do
      lx "c'e' c'1'" `shouldBe` [LiteralChar 101, LiteralChar 49]
    it "lexes negative int literals" $ do
      lx "-123 -456" `shouldBe` map (\x -> LiteralInt x Nothing) [-123, -456]
    it "lexes suffixed int literals" $ do
      lx "1_i64 234_u8 0_f32"
        `shouldBe` [ LiteralInt 1   (Just Int64)
                   , LiteralInt 234 (Just Uint8)
                   , LiteralInt 0   (Just Float32)
                   ]
    it "lexes hex int literals" $ do
      lx "0x123abcf" `shouldBe` [LiteralInt 19114959 Nothing]
    it "lexes octal int literals" $ do
      lx "0o701" `shouldBe` [LiteralInt 449 Nothing]
    it "lexes binary int literals" $ do
      lx "0b101" `shouldBe` [LiteralInt 5 Nothing]
    it "lexes string literals" $ do
      lx "'abc' \"def\" \"\"\"ghi\njkl\"\"\""
        `shouldBe` [ LiteralString "abc"
                   , LiteralString "def"
                   , LiteralString ("ghi\njkl")
                   ]
    it "lexes special characters in string literals" $ do
      lx "'\\n'" `shouldBe` [LiteralString "\n"]
      lx "'\\r'" `shouldBe` [LiteralString "\r"]
      lx "'\\t'" `shouldBe` [LiteralString "\t"]
      lx "'\\b'" `shouldBe` [LiteralString "\b"]
      lx "'\\\\n'" `shouldBe` [LiteralString "\\n"]
    it "lexes escaped characters in string literals" $ do
      lx "\"a\\\"\\Bc\"" `shouldBe` [LiteralString "a\"Bc"]
    it "lexes float literals" $ do
      lx "0.1 0.10000 -1.0_f64"
        `shouldBe` [ LiteralFloat ("0.1")     Nothing
                   , LiteralFloat ("0.10000") Nothing
                   , LiteralFloat ("-1.0")    (Just Float64)
                   ]
    it "lexes bool literals" $ do
      lx "true false" `shouldBe` [LiteralBool True, LiteralBool False]
    it "lexes keywords" $ do
      lx "public in inline this match rule rules"
        `shouldBe` [ KeywordPublic
                   , KeywordIn
                   , KeywordInline
                   , KeywordThis
                   , KeywordMatch
                   , KeywordRule
                   , KeywordRules
                   ]
    it "lexes function call block" $ do
      lx "  {\nf.ghi(abc)\n}"
        `shouldBe` [ CurlyBraceOpen
                   , LowerIdentifier "f"
                   , Dot
                   , LowerIdentifier "ghi"
                   , ParenOpen
                   , LowerIdentifier "abc"
                   , ParenClose
                   , CurlyBraceClose
                   ]
    it "lexes metadata" $ do
      lx "#[abc][]"
        `shouldBe` [ MetaOpen
                   , LowerIdentifier "abc"
                   , SquareBraceClose
                   , SquareBraceOpen
                   , SquareBraceClose
                   ]
