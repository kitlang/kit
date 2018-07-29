{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Kit.Parser.Token where

import Kit.Str
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
  | DocComment Str
  | KeywordAbstract
  | KeywordAs
  | KeywordAtom
  | KeywordBreak
  | KeywordConst
  | KeywordContinue
  | KeywordDefault
  | KeywordDefer
  | KeywordDo
  | KeywordElse
  | KeywordEnum
  | KeywordFor
  | KeywordFunction
  | KeywordIf
  | KeywordImplement
  | KeywordImport
  | KeywordInclude
  | KeywordInline
  | KeywordIn
  | KeywordMatch
  | KeywordNull
  | KeywordPrivate
  | KeywordPublic
  | KeywordReturn
  | KeywordRule
  | KeywordRules
  | KeywordSelf
  | KeywordSpecialize
  | KeywordStatic
  | KeywordStruct
  | KeywordThen
  | KeywordThis
  | KeywordThrow
  | KeywordTrait
  | KeywordTypedef
  | KeywordUnion
  | KeywordUnsafe
  | KeywordVar
  | KeywordWhile
  | LiteralBool Bool
  | LiteralString Str
  | LiteralFloat Str
  | LiteralInt Str
  | Op Operator
  | Lex Str
  | LowerIdentifier Str
  | MacroIdentifier Str
  | UpperIdentifier Str
  deriving (Eq)

instance Show TokenClass where
  show tok = case tok of
    MetaOpen -> "#["
    ParenOpen -> "("
    ParenClose -> ")"
    CurlyBraceOpen -> "{"
    CurlyBraceClose -> "}"
    SquareBraceOpen -> "["
    SquareBraceClose -> "]"
    Comma -> ","
    Colon -> ":"
    Semicolon -> ";"
    TripleDot -> "..."
    Dot -> "."
    Hash -> "#"
    Dollar -> "$"
    Arrow -> "=>"
    Question -> "?"
    DocComment _ -> "/** doc comment */"
    KeywordAbstract -> "abstract"
    KeywordAs -> "as"
    KeywordAtom -> "atom"
    KeywordBreak -> "break"
    KeywordConst -> "const"
    KeywordContinue -> "continue"
    KeywordDefault -> "default"
    KeywordDefer -> "defer"
    KeywordDo -> "do"
    KeywordElse -> "else"
    KeywordEnum -> "enum"
    KeywordFor -> "for"
    KeywordFunction -> "function"
    KeywordIf -> "if"
    KeywordImplement -> "implement"
    KeywordImport -> "import"
    KeywordInclude -> "include"
    KeywordInline -> "inline"
    KeywordIn -> "in"
    KeywordMatch -> "match"
    KeywordNull -> "null"
    KeywordPrivate -> "private"
    KeywordPublic -> "public"
    KeywordReturn -> "return"
    KeywordRule -> "rule"
    KeywordRules -> "rules"
    KeywordSelf -> "self"
    KeywordSpecialize -> "specialize"
    KeywordStatic -> "static"
    KeywordStruct -> "struct"
    KeywordThen -> "then"
    KeywordThis -> "this"
    KeywordThrow -> "throw"
    KeywordTrait -> "trait"
    KeywordTypedef -> "typedef"
    KeywordUnion -> "union"
    KeywordUnsafe -> "unsafe"
    KeywordVar -> "var"
    KeywordWhile -> "while"
    LiteralBool True -> "bool `true`"
    LiteralBool False -> "bool `false`"
    LiteralString s -> "string literal `" ++ (s_unpack s) ++ "`"
    LiteralFloat s -> "float literal `" ++ (s_unpack s) ++ "`"
    LiteralInt s -> "int literal `" ++ (s_unpack s) ++ "`"
    Op op -> "operator " ++ show op
    Lex s -> "lex macro `" ++ (s_unpack s) ++ "!`"
    LowerIdentifier s -> "identifier `" ++ (s_unpack s) ++ "`"
    MacroIdentifier s -> "macro identifier `" ++ (s_unpack s) ++ "`"
    UpperIdentifier s -> "type constructor `" ++ (s_unpack s) ++ "`"
