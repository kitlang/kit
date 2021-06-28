#pragma once

#include "../maybe.h"
#include "../string.h"

struct Position {
  int line;
  int column;
};
typedef struct Position Position;

struct Span {
  String file;
  Position start;
  Position end;
};
typedef struct Span Span;

Span NoPos = { NilString, { 0, 0 }, { 0, 0 } };

Maybe(Span);
