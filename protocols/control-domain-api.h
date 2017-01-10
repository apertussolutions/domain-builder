// BANNERSTART
// Copyright: 2011 Galois Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki (diatchki@galois.com)
// BANNEREND

#include "prelude.h"

typedef struct {
  size_t schema_length;
  uint8_t schema_data[schema_length];
} vp_specification;

typedef struct {
  char *name;
  size_t spec_length;
  uint8_t spec[spec_length];
} vm_specification;

// The domain id, or -ve error code.
typedef uint64_t vm_specification_res;

typedef struct {
  evtchn_port_t port;
  grant_ref_t   ref;
} conn_request_t;

// This is what's in the shared memory.
typedef union {
    vp_specification create_vp_arguments;
    vm_specification create_vm_arguments;
    struct {} list_domains;
    struct {} terminate_vp;
    int shutdown_vm;
    struct {} measure_vp;
} control_command __attribute__((codec));


typedef struct {
  uint64_t domid;
  char*    vm_name;
} cd_list_entry;

typedef GENERATE(encode_list_result, decode_list_result)
struct {
    size_t len;
    cd_list_entry domains[len];
} list_domains_res;
