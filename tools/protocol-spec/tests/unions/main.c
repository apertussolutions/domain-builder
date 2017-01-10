#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include "types.h"
#include "pretty.h"
#include "encode.h"
#include "decode.h"

static int runTest(const char *file, test_type *cmd) {

  char buf[1000] = {0};
  test_type *view = (test_type*)buf;

  printf("-- %s -----------------\n",file);
  pretty_test_type(0,cmd);

  printf("encoding: ");
  int sz = encode(cmd, buf, sizeof(buf));
  if (sz < 0) {
    printf("Error %d\n", sz);
    return sz;
  } else {
    printf("Ok (%d bytes)\n", sz);
  }

  printf("saving: ");
  int fd = creat(file,0666);
  write(fd,buf,sz);
  close(fd);
  printf("Ok\n");

  printf("decoding: ");
  memset(buf + sz,0xC2,sizeof(buf) - sz);   // Fill with garbage after size.
  int err = decode(&view, sz);
  if (err != 0) {
    printf("Error %d\n", err);
    return sz;
  } else {
  printf("Ok\n");
  }

  pretty_test_type(0,view);
  printf("------------------------------------\n\n");
  return 0;
}

int main() {

  test_type cmd;

  cmd.var1              = "first field";
  cmd.var2.tag          = branch1;
  cmd.var2.u.branch1.x  = 1234567890;
  cmd.var2.u.branch1.y  = 2068013579;
  cmd.var3              = "last field";

  runTest("test1.out", &cmd);

  cmd.var2.tag          = branch2;
  cmd.var2.u.branch2    = 1122334455;

  runTest("test2.out", &cmd);

  return 0;
}
