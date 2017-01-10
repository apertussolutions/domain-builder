#include "prelude.h"

typedef GENERATE(encode_cmd, decode_cmd)
struct {
  uint64_t timestamp;

  union TAGPREFIX(cmd) {
    struct {
      char *filename;
      unsigned int num_refs;
      grant_ref_t refs[num_refs];
    } open;

    struct {
      uint64_t fd;
    } close;

    struct {
      uint64_t fd;
      uint64_t offset;
    } read;

    struct {
    } disconnect;
  } cmd;
} host_storage_cmd_t;

typedef GENERATE(encode_res, decode_res)
struct {
  uint64_t timestamp;
  union TAGPREFIX(res) {
    struct {
      uint64_t fd;
      uint64_t size;
      char *sec_ctx;
    } open;

    struct {
      uint64_t size;
    } read;

    uint64_t error;

    struct {
    } ok;
  } res;
} host_storage_res_t;
