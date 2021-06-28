#pragma once

#include <stdlib.h>

enum Operator {
  Assign,
  Inc,
  Dec,
  Add,
  Sub,
  Div,
  Mul,
  Mod,
  Eq,
  Neq,
  Gte,
  Lte,
  LeftShift,
  RightShift,
  Gt,
  Lt,
  And,
  Or,
  BitAnd,
  BitOr,
  BitXor,
  Invert,
  InvertBits,
  Cons,
};
typedef enum Operator Operator;

char* showOperator(Operator op) {
  switch (op)
  {
  case Assign:
    return "op =";
  case Inc:
    return "op ++";
  case Dec:
    return "op --";
  case Add:
    return "op +";
  case Sub:
    return "op -";
  case Div:
    return "op /";
  case Mul:
    return "op *";
  case Mod:
    return "op %";
  case Eq:
    return "op =";
  case Neq:
    return "op !=";
  case Gte:
    return "op >=";
  case Lte:
    return "op <=";
  case LeftShift:
    return "op <<";
  case RightShift:
    return "op >>";
  case Gt:
    return "op >";
  case Lt:
    return "op <";
  case And:
    return "op &&";
  case Or:
    return "op ||";
  case BitAnd:
    return "op &";
  case BitOr:
    return "op |";
  case BitXor:
    return "op ^";
  case Invert:
    return "op !";
  case InvertBits:
    return "op ~";
  case Cons:
    return "op ::";
  default:
    return NULL;
  }
}
