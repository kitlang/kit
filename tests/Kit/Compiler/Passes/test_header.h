// types

struct Struct1 {
  char field1;
  unsigned short field2;
};

typedef struct {
  int field1;
  double field2;
} Struct2;

typedef struct Struct3 Struct3;

enum Enum1 {
  apple,
  banana,
  strawberry,
};

// variables

extern int var1;
char var2 = 1;
unsigned long var3;
struct Struct2 struct_var1;
enum Enum1 enum_var1;
int *pointer_var1;
int **pointer_var2;
int *pointer_var3(int arg1);

// functions

void void_func1();
int int_func1();
float func_with_args(int arg1, unsigned long long arg2);
struct Struct1 struct_func(struct Struct2 a);
float (*pointer_func)(int *arg1);
void varargs_func(int a, ...);
