{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Kit.Parser.Token where

import Data.Hashable
import GHC.Generics
import Kit.Str
import Kit.Ast.Operator
import Kit.Ast.Span

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
  | FunctionArrow
  | Question
  | Underscore
  | WildcardSuffix
  | DoubleWildcardSuffix
  | KeywordAbstract
  | KeywordAs
  | KeywordBreak
  | KeywordConst
  | KeywordContinue
  | KeywordDefault
  | KeywordDefer
  | KeywordDo
  | KeywordElse
  | KeywordEmpty
  | KeywordEnum
  | KeywordExtend
  | KeywordFor
  | KeywordFunction
  | KeywordIf
  | KeywordImplement
  | KeywordImplicit
  | KeywordImport
  | KeywordInclude
  | KeywordInline
  | KeywordIn
  | KeywordMacro
  | KeywordMatch
  | KeywordNull
  | KeywordPrivate
  | KeywordPublic
  | KeywordReturn
  | KeywordRule
  | KeywordRules
  | KeywordSelf
  | KeywordSizeof
  | KeywordSpecialize
  | KeywordStatic
  | KeywordStruct
  | KeywordThen
  | KeywordThis
  | KeywordThrow
  | KeywordTokens
  | KeywordTrait
  | KeywordTypedef
  | KeywordUndefined
  | KeywordUnion
  | KeywordUnsafe
  | KeywordUsing
  | KeywordVar
  | KeywordWhile
  | KeywordYield
  | LiteralChar Int
  | LiteralBool Bool
  | LiteralString Str
  | LiteralFloat Str (Maybe NumSpec)
  | LiteralInt Int (Maybe NumSpec)
  | Op Operator
  | Lex Str
  | LowerIdentifier Str
  | MacroIdentifier Str
  | UpperIdentifier Str
  | InlineC Str
  deriving (Eq, Generic)

instance Hashable TokenClass
instance Hashable NumSpec

data NumSpec = CChar | CInt | CSize | Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32 | Uint64 | Float32 | Float64 deriving (Eq, Generic)
instance Show NumSpec where
  show CChar = "Char"
  show CInt = "Int"
  show CSize = "Size"
  show Int8 = "Int8"
  show Int16 = "Int16"
  show Int32 = "Int32"
  show Int64 = "Int64"
  show Uint8 = "Uint8"
  show Uint16 = "Uint16"
  show Uint32 = "Uint32"
  show Uint64 = "Uint64"
  show Float32 = "Float32"
  show Float64 = "Float64"

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
    FunctionArrow -> "->"
    Question -> "?"
    Underscore -> "_"
    WildcardSuffix -> ".*"
    DoubleWildcardSuffix -> ".**"
    KeywordAbstract -> "abstract"
    KeywordAs -> "as"
    KeywordBreak -> "break"
    KeywordConst -> "const"
    KeywordContinue -> "continue"
    KeywordDefault -> "default"
    KeywordDefer -> "defer"
    KeywordDo -> "do"
    KeywordElse -> "else"
    KeywordEmpty -> "empty"
    KeywordEnum -> "enum"
    KeywordExtend -> "extend"
    KeywordFor -> "for"
    KeywordFunction -> "function"
    KeywordIf -> "if"
    KeywordImplement -> "implement"
    KeywordImplicit -> "implicit"
    KeywordImport -> "import"
    KeywordInclude -> "include"
    KeywordInline -> "inline"
    KeywordIn -> "in"
    KeywordMacro -> "macro"
    KeywordMatch -> "match"
    KeywordNull -> "null"
    KeywordPrivate -> "private"
    KeywordPublic -> "public"
    KeywordReturn -> "return"
    KeywordRule -> "rule"
    KeywordRules -> "rules"
    KeywordSelf -> "self"
    KeywordSizeof -> "sizeof"
    KeywordSpecialize -> "specialize"
    KeywordStatic -> "static"
    KeywordStruct -> "struct"
    KeywordThen -> "then"
    KeywordThis -> "this"
    KeywordThrow -> "throw"
    KeywordTokens -> "tokens"
    KeywordTrait -> "trait"
    KeywordTypedef -> "typedef"
    KeywordUndefined -> "undefined"
    KeywordUnion -> "union"
    KeywordUnsafe -> "unsafe"
    KeywordUsing -> "using"
    KeywordVar -> "var"
    KeywordWhile -> "while"
    KeywordYield -> "yield"
    LiteralBool True -> "bool `true`"
    LiteralBool False -> "bool `false`"
    LiteralString s -> "string literal `" ++ s_unpack s ++ "`"
    LiteralFloat s _ -> "float literal `" ++ s_unpack s ++ "`"
    LiteralInt s _ -> "int literal `" ++ show s ++ "`"
    LiteralChar c -> "char literal `" ++ show c ++ "`"
    Op op -> "operator " ++ show op
    Lex s -> "lex macro `" ++ s_unpack s ++ "!`"
    LowerIdentifier s -> "identifier `" ++ s_unpack s ++ "`"
    MacroIdentifier s -> "macro identifier `" ++ s_unpack s ++ "`"
    UpperIdentifier s -> "type constructor `" ++ s_unpack s ++ "`"
    InlineC s -> "inline c `" ++ s_unpack s ++ "`"
