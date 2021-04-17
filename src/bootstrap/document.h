#pragma once

#include "ast/span.h"
#include "string.h"

struct Document {
  String file;
  String contents;
  size_t lineLength;
};
typedef struct Document Document;

Span posToSpan(Document document, int position) {
  // TODO: Calculate span given offset
  return NoPos;
}
