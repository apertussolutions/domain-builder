#include "zlib.h"

int main() {
  inflateInit2(0, 0);
  inflate(0, 0);
  inflateEnd(0);
  return 0;
}
