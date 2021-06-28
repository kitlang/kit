#pragma once

#include <stdlib.h>

enum NumSpec {
  CChar,
  CInt,
  CSize,
  Int8,
  Int16,
  Int32,
  Int64,
  Uint8,
  Uint16,
  Uint32,
  Uint64,
  Float32,
  Float64
};
typedef enum NumSpec NumSpec;

char* showNumSpec(NumSpec numSpec) {
  switch (numSpec) {
  case CChar:
    return "Char";
  case CInt:
    return "Int";
  case CSize:
    return "Size";
  case Int8:
    return "Int8";
  case Int16:
    return "Int16";
  case Int32:
    return "Int32";
  case Int64:
    return "Int64";
  case Uint8:
    return "Uint8";
  case Uint16:
    return "Uint16";
  case Uint32:
    return "Uint32";
  case Uint64:
    return "Uint64";
  case Float32:
    return "Float32";
  case Float64:
    return "Float64";
  default:
    return NULL;
  }
}
