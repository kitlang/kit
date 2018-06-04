module Kit.Parser.Token where

  import qualified Data.ByteString.Lazy.Char8 as B
  import Kit.Ast.Operator
  import Kit.Parser.Span

  type Token = (TokenClass, Span)

  data TokenClass
    = MetaOpen
    | ParenOpen
    | ParenClose
    | CurlyBraceOpen
    | CurlyBraceClose
    | SquareBraceOpen
    | SquareBraceClose
    | Comma
    | Colon
    | Semicolon
    | TripleDot
    | Dot
    | Hash
    | Dollar
    | Arrow
    | Question
    | DocComment B.ByteString
    | KeywordAbstract
    | KeywordAs
    | KeywordAtom
    | KeywordBreak
    | KeywordCase
    | KeywordCode
    | KeywordContinue
    | KeywordDefault
    | KeywordDo
    | KeywordElse
    | KeywordEnum
    | KeywordFor
    | KeywordFunction
    | KeywordIf
    | KeywordImplement
    | KeywordImport
    | KeywordInline
    | KeywordIn
    | KeywordMacro
    | KeywordMatch
    | KeywordNew
    | KeywordOp
    | KeywordOverride
    | KeywordPrivate
    | KeywordPublic
    | KeywordReturn
    | KeywordRule
    | KeywordRules
    | KeywordSelf
    | KeywordStruct
    | KeywordSuper
    | KeywordSwitch
    | KeywordThen
    | KeywordThis
    | KeywordThrow
    | KeywordToken
    | KeywordTrait
    | KeywordUnsafe
    | KeywordVar
    | KeywordWhile
    | LiteralBool Bool
    | LiteralString B.ByteString
    | LiteralFloat B.ByteString
    | LiteralInt B.ByteString
    | Op Operator
    | Lex B.ByteString
    | LowerIdentifier B.ByteString
    | UpperIdentifier B.ByteString
    deriving (Eq, Show)
