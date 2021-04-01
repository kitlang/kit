#pragma once

#include <stdlib.h>

enum Operator {
  Plus,
  Minus,
  Division,
  Modulo,
};
typedef enum Operator Operator;

char* showOperator(Operator op) {
  switch (op)
  {
  case Plus:
    return "op +";
  case Minus:
    return "op -";
  case Division:
    return "op /";
  case Modulo:
    return "op %";
  default:
    return NULL;
  }
}
