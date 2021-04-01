#pragma once

#include "lexer.h"

struct ParseError {
  /// Whether this error represents a compiler assertion failure; should always be reported.
  bool internal;
  // TODO: Support other types of errors, i.e. `Parse`, `Internal`, and `Import` error types
  String message;
  MaybeOf(Span) pos;
};
typedef struct ParseError ParseError;

struct KitErrors {
  int length;
  ParseError* errors;
};
typedef struct KitErrors KitErrors;

bool parseString(String module, KitErrors* errors) {
  Token* tokens;
  if (!tokenize(module, tokens)) return false;

  // TODO: Port a basic parser from Haskell to convert token list to an AST

  return true;
}

// TODO: Add `parseFile` function ()
