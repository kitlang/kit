#pragma once

#include "ast/span.h"
#include "string.h"

struct Document {
  String file;
  String contents;
  size_t lineLength;
};
typedef struct Document Document;

Position posToPosition(Document* document, int pos) {
  Position position;

  int line = 0;
  int col = 0;
  for (int i = 0; i <= pos; i += 1) {
    if (document->contents.cString[i] == '\n') {
      line++;
      col = 0;
    }
    else col += 1;
  }
  position.line = line;
  position.column = col;

  return position;
}

Span posToSpan(Document* document, int startPos, int endPos) {
  Position start = posToPosition(document, startPos);
  Position end = posToPosition(document, endPos);
  return (Span) { document->file, start, end };
}
