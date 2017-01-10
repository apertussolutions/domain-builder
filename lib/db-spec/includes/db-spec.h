#ifndef  DB_SPEC_VALIDATE_H
#define DB_SPEC_VALIDATE_H

#include <db-api/types.h>
#include <db-api/encode.h>

typedef enum
  { dbcf_no_error
  , dbcf_no_cpu
  , dbcf_no_memory
  , dbcf_bad_maxmemory
  , dbcf_command_line_too_long
  , dbcf_no_kernel
  , dbcf_no_ramdisk
  } db_boot_spec_error;

// Parse and validate a configuration.
// Returns a negative number if a structural parse error occured
// (indicating garbage configuration), or a positive db_config_error
// if a semantic error was detected (indicating an incorrect configuration).
int db_boot_spec_validate(db_boot_spec **bytes, size_t size);
int db_build_request_validate(db_build_request **bytes, size_t size);
char * db_boot_spec_error_show(db_boot_spec_error err);

#endif

