#pragma once

#include <stdbool.h>

struct Maybe_t {
  bool isNull;
};

/// Declare an optional type derived from `T`.
#define Maybe(T) struct Maybe_##T { struct Maybe_t _state; T* value; }; \
typedef struct Maybe_##T Maybe_##T;

/// The type of an optional type derived from `T`.
#define MaybeOf(T) Maybe_##T
