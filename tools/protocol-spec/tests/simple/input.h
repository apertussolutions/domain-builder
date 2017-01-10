#define uint64_t unsigned long
#define grant_ref_t unsigned
#define TAGPREFIX(x) __attribute__((tagprefix(x)))
#define GENERATE(x,y) __attribute__((codec(x,y)))

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
  } cmd;
} host_storage_cmd_t;
