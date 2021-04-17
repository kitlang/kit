#pragma once

#include "../maybe.h"
#include "../string.h"

struct Span {
  String file;
  int startLine;
  int startCol;
  int endLine;
  int endCol;
};
typedef struct Span Span;

Span NoPos = { NilString, 0, 0, 0, 0 };

Maybe(Span);
