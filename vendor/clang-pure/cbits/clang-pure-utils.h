/*
Copyright 2014 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

typedef void (*haskell_visitor)(CXCursor*);

// Macro that evaluates to a copy on the heap of the result of a given expression.
#define ALLOC(__ALLOC_EXPR__) ({\
  typeof (__ALLOC_EXPR__) __alloc_res__ = (__ALLOC_EXPR__);\
  typeof (__ALLOC_EXPR__) *__alloc_ptr__ = malloc(sizeof(__alloc_res__));\
  *__alloc_ptr__ = __alloc_res__;\
  __alloc_ptr__;\
  })

// Traverse children using a haskell_visitor passed in as client_data.
// The visitor gets a copy of the cursor on the heap and is responsible
// for freeing it.
static enum CXChildVisitResult visit_haskell(CXCursor cursor,
                                             CXCursor parent,
                                             CXClientData client_data) {
  ((haskell_visitor) client_data)(ALLOC(cursor));
  return CXChildVisit_Continue;
};
