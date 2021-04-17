#pragma once

#include <assert.h>
#include <stdbool.h>

#include "../ast/numbers.h"
#include "../ast/operators.h"
#include "../ast/span.h"
// #include "keywords.h"
#include "../maybe.h"

enum TokenClass {
  MetaOpen,
  ParenOpen,
  ParenClose,
  CurlyBraceOpen,
  CurlyBraceClose,
  SquareBraceOpen,
  SquareBraceClose,
  Comma,
  Colon,
  Semicolon,
  TripleDot,
  Dot,
  Hash,
  Dollar,
  Arrow,
  FunctionArrow,
  Question,
  Underscore,
  WildcardSuffix,
  DoubleWildcardSuffix,
  KeywordAbstract,
  KeywordAs,
  KeywordBreak,
  KeywordConst,
  KeywordContinue,
  KeywordDefault,
  KeywordDefined,
  KeywordDefer,
  KeywordDo,
  KeywordElse,
  KeywordEmpty,
  KeywordEnum,
  KeywordExtend,
  KeywordFor,
  KeywordFunction,
  KeywordIf,
  KeywordImplement,
  KeywordImplicit,
  KeywordImport,
  KeywordInclude,
  KeywordInline,
  KeywordIn,
  KeywordMacro,
  KeywordMatch,
  KeywordNull,
  KeywordPrivate,
  KeywordPublic,
  KeywordReturn,
  KeywordRule,
  KeywordRules,
  KeywordSelf,
  KeywordSizeof,
  KeywordSpecialize,
  KeywordStatic,
  KeywordStruct,
  KeywordThen,
  KeywordThis,
  KeywordThrow,
  KeywordTokens,
  KeywordTrait,
  KeywordTypedef,
  KeywordUndefined,
  KeywordUnion,
  KeywordUnsafe,
  KeywordUsing,
  KeywordVar,
  KeywordWhile,
  KeywordYield,
  // Tokens with data
  LiteralChar,
  LiteralBool,
  LiteralString,
  LiteralFloat,
  LiteralInt,
  Op,
  AssignOp,
  CustomOp,
  Lex,
  LowerIdentifier,
  MacroIdentifier,
  UpperIdentifier,
  InlineC,
};
typedef enum TokenClass TokenClass;

struct Token {
  TokenClass type;
  struct Span span;
  void* data;
};
typedef struct Token Token;

#define TokenData(T, members) struct T##Data members; \
typedef struct T##Data T##Data;

// Data structures for tokens with data
TokenData(LiteralChar, {
  char value;
});
TokenData(LiteralBool, {
  bool value;
});
TokenData(LiteralString, {
  String value;
});
Maybe(NumSpec);
TokenData(LiteralFloat, {
  String value;
  MaybeOf(NumSpec) type;
});
TokenData(LiteralInt, {
  int value;
  MaybeOf(NumSpec) type;
});
TokenData(Op, {
  Operator operator;
});
TokenData(AssignOp, {
  Operator operator;
});
TokenData(CustomOp, {
  String operator;
});
// TODO: WTF is a Lex token? Some macro shit?
TokenData(Lex, {
  String macro;
});
TokenData(LowerIdentifier, {
  String value;
});
TokenData(MacroIdentifier, {
  String value;
});
TokenData(UpperIdentifier, {
  String value;
});
TokenData(InlineC, {
  String source;
});

const char* showToken(Token token) {
  switch (token.type) {
    case MetaOpen: return "#[";
    case ParenOpen: return "(";
    case ParenClose: return ")";
    case CurlyBraceOpen: return "{";
    case CurlyBraceClose: return "}";
    case SquareBraceOpen: return "[";
    case SquareBraceClose: return "]";
    case Comma: return ",";
    case Colon: return ":";
    case Semicolon: return ";";
    case TripleDot: return "...";
    case Dot: return ".";
    case Hash: return "#";
    case Dollar: return "$";
    case Arrow: return "=>";
    case FunctionArrow: return "->";
    case Question: return "?";
    case Underscore: return "_";
    case WildcardSuffix: return ".*";
    case DoubleWildcardSuffix: return ".**";
    case KeywordAbstract: return "abstract";
    case KeywordAs: return "as";
    case KeywordBreak: return "break";
    case KeywordConst: return "const";
    case KeywordContinue: return "continue";
    case KeywordDefault: return "default";
    case KeywordDefer: return "defer";
    case KeywordDefined: return "defined";
    case KeywordDo: return "do";
    case KeywordElse: return "else";
    case KeywordEmpty: return "empty";
    case KeywordEnum: return "enum";
    case KeywordExtend: return "extend";
    case KeywordFor: return "for";
    case KeywordFunction: return "function";
    case KeywordIf: return "if";
    case KeywordImplement: return "implement";
    case KeywordImplicit: return "implicit";
    case KeywordImport: return "import";
    case KeywordInclude: return "include";
    case KeywordInline: return "inline";
    case KeywordIn: return "in";
    case KeywordMacro: return "macro";
    case KeywordMatch: return "match";
    case KeywordNull: return "null";
    case KeywordPrivate: return "private";
    case KeywordPublic: return "public";
    case KeywordReturn: return "return";
    case KeywordRule: return "rule";
    case KeywordRules: return "rules";
    case KeywordSelf: return "self";
    case KeywordSizeof: return "sizeof";
    case KeywordSpecialize: return "specialize";
    case KeywordStatic: return "static";
    case KeywordStruct: return "struct";
    case KeywordThen: return "then";
    case KeywordThis: return "this";
    case KeywordThrow: return "throw";
    case KeywordTokens: return "tokens";
    case KeywordTrait: return "trait";
    case KeywordTypedef: return "typedef";
    case KeywordUndefined: return "undefined";
    case KeywordUnion: return "union";
    case KeywordUnsafe: return "unsafe";
    case KeywordUsing: return "using";
    case KeywordVar: return "var";
    case KeywordWhile: return "while";
    case KeywordYield: return "yield";
    // Tokens with data
    case LiteralBool:
      assert(token.data != NULL);
      LiteralBoolData* literalBool = ((LiteralBoolData*) token.data);
      if (literalBool->value) return "bool `true`";
      else return "bool `false`";
    case LiteralString:
      assert(token.data != NULL);
      LiteralStringData* literalString = ((LiteralStringData*) token.data);
      return concatStrings(3, "string literal `", literalString->value.cString, "`").cString;
    case LiteralFloat:
      assert(token.data != NULL);
      LiteralFloatData* literalFloat = ((LiteralFloatData*) token.data);
      return concatStrings(3, "float literal `", literalFloat->value.cString, "`").cString;
    case LiteralInt:
      assert(token.data != NULL);
      LiteralIntData* literalInt = ((LiteralIntData*) token.data);
      return concatStrings(3, "int literal `", intToString(literalInt->value).cString, "`").cString;
    case LiteralChar:
      assert(token.data != NULL);
      LiteralCharData* literalChar = ((LiteralCharData*) token.data);
      return concatStrings(3, "char literal `", toString(&(literalChar->value)).cString, "`").cString;
    case Op:
      assert(token.data != NULL);
      OpData* op = ((OpData*) token.data);
      return concatStrings(2, "operator ", showOperator(op->operator)).cString;
    case Lex:
      assert(token.data != NULL);
      LexData* lex = ((LexData*) token.data);
      return concatStrings(3, "lex macro `", lex->macro.cString, "!`").cString;
    case LowerIdentifier:
      assert(token.data != NULL);
      LowerIdentifierData* lowerIdentifier = ((LowerIdentifierData*) token.data);
      return concatStrings(3, "identifier `", lowerIdentifier->value.cString, "`").cString;
    case MacroIdentifier:
      assert(token.data != NULL);
      MacroIdentifierData* macroIdentifier = ((MacroIdentifierData*) token.data);
      return concatStrings(3, "macro identifier `", macroIdentifier->value.cString, "`").cString;
    case UpperIdentifier:
      assert(token.data != NULL);
      UpperIdentifierData* upperIdentifier = ((UpperIdentifierData*) token.data);
      return concatStrings(3, "type constructor `", upperIdentifier->value.cString, "`").cString;
    case InlineC:
      assert(token.data != NULL);
      InlineCData* inlineC = ((InlineCData*) token.data);
      return concatStrings(3, "inline c `", inlineC->source.cString, "`").cString;
    default:
      assert(0);
      break;
  }
}
