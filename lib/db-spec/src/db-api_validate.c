#include <db-spec.h>
#include <db-api/decode.h>
#include <stdint.h>
#include <xen/xen.h>
#include <string.h>

#define SHA256_HASH_SIZE 32

char *db_boot_spec_error_show(db_boot_spec_error err) {

  switch (err) {
    case dbcf_no_cpu:              return "Number of CPUs not specified";
    case dbcf_no_memory:           return "Amount of memory not specified";
    case dbcf_bad_maxmemory:       return "Amount of maximum memory too low";
    case dbcf_no_kernel:           return "Kernel image not specified";
    case dbcf_no_ramdisk:          return "Missing RAM disk, but hash present";
    case dbcf_command_line_too_long: return "Command line is too long";
    default:                       return "Unknown error";
  }
}



// Additional semantic validation.
static
db_boot_spec_error db_config_val(db_boot_spec *msg) {

  // We have at least one CPU
  if (msg->spec.max_vcpus == 0) return dbcf_no_cpu;

  // We have some memory
  if (msg->spec.allocate_kbs == 0) return dbcf_no_memory;

  // We have more max memory
  if (msg->spec.allocate_kbs > msg->spec.max_kbs) return dbcf_bad_maxmemory;

  // Command line
  if ( msg->spec.command_line != NULL
    && strlen(msg->spec.command_line) >= MAX_GUEST_CMDLINE)
      return dbcf_command_line_too_long;

  // We have a kernel
  if (msg->kernel_name == NULL) return dbcf_no_kernel;

  return dbcf_no_error;
}


int db_boot_spec_validate(db_boot_spec **bytes, size_t size) {
  int err;

  err = db_boot_spec_decode((void**)bytes, size);
  if (err != 0) return err;

  return db_config_val(*bytes);
}


int db_build_request_validate(db_build_request **bytes, size_t size) {
  return db_build_request_decode((void**)bytes, size);
}




