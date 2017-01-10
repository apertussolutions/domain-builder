#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include "types.h"
#include "pretty.h"
#include "encode.h"
#include "decode.h"

static int runTest(const char *file, host_storage_cmd_t *cmd) {

  char buf[1000] = {0};
  host_storage_cmd_t *view = (host_storage_cmd_t*)buf;

  printf("-- %s -----------------\n",file);
  pretty_host_storage_cmd_t(0,cmd);

  printf("encoding: ");
  fflush(stdout);

  int sz = encode_cmd(cmd, buf, sizeof(buf));
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
  int err = decode_cmd(&view, sz);
  if (err != 0) {
    printf("Error %d\n", err);
    return sz;
  } else {
  printf("Ok\n");
  }

  pretty_host_storage_cmd_t(0,view);
  printf("------------------------------------\n\n");
  return 0;
}

int main() {

  int refs[] = {20, 30};

  host_storage_cmd_t cmd;

  cmd.timestamp         = 1234;
  cmd.cmd.tag           = cmdopen;
  cmd.cmd.u.open.filename = "/path/to/file";
  cmd.cmd.u.open.num_refs = 2;
  cmd.cmd.u.open.refs     = refs;

  runTest("test1.out", &cmd);

  cmd.timestamp         = 4321;
  cmd.cmd.tag           = cmdopen;
  cmd.cmd.u.open.filename = NULL;
  cmd.cmd.u.open.num_refs = 0;
  cmd.cmd.u.open.refs     = NULL;

  runTest("test2.out", &cmd);

  cmd.timestamp         = 5432;
  cmd.cmd.tag           = cmdclose;
  cmd.cmd.u.close.fd      = 42;

  runTest("test3.out", &cmd);

  return 0;
}
