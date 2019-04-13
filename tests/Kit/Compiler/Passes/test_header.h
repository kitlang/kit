// types

struct Struct1 {
  char field1;
  unsigned short field2;
};

typedef struct {
  short field1;
  double field2;
} Struct2;

typedef struct Struct3 Struct3;

enum Enum1 {
  apple,
  banana,
  cherry
};

typedef enum {
  kiwi,
  lime,
  mango
} Enum2;

// variables

extern short var1;
char var2 = 1;
unsigned long var3;
struct Struct1 struct_var1;
enum Enum1 enum_var1;
short *pointer_var1;
short **pointer_var2;
short (*pointer_var3)(short arg1);
void (*func_pointer)(void);
unsigned just_unsigned;

// functions

void void_func1();
short int_func1();
float func_with_args(short arg1, unsigned long long arg2);
struct Struct1 struct_func(struct Struct2 a);
float * pointer_func(int *arg1);
void varargs_func(short a, ...);
long void_func(void);
int fake_atexit (void (*__func) (void));
