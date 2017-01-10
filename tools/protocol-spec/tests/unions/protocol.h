#define TAGPREFIX(x) __attribute__((tagprefix(x)))
#define GENERATE(x,y) __attribute__((codec(x,y)))

typedef GENERATE(encode, decode)
struct {
  char * var1;

  union {
    struct {
      int x;
      int y;
    } branch1;

    int branch2;
  } var2;

  char * var3;
} test_type;
