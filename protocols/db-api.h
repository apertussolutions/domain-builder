// This is the basic format for building VMs.
// It is used by db-boot and db-server.

#include "prelude.h"

typedef struct {
  uint64_t start;
  uint64_t how_many;
  uint64_t enabled;
} db_spec_range;


typedef struct {
  size_t iomem_count;
  db_spec_range iomem[iomem_count];

  size_t ioport_count;
  db_spec_range ioport[ioport_count];

  size_t irq_bitmap_size;
  uint8_t irq_bitmap[irq_bitmap_size];
} db_perms_t;


typedef struct {
  uint64_t domid;              // fixed domid, or 0 for auto-pick.
  uint64_t max_vcpus;          // Maximum number of CPUs
  uint64_t allocate_kbs;       // physical memory, in KB
  uint64_t max_kbs;            // maximum physical memory, in KB
  uint64_t dom_flags;          // SIF flags
  char *command_line;

  union {
    struct {}  hvm;
    db_perms_t pv;
  } vm;

} db_spec;



// -----------------------------------------------------------------------------
// Specs used by db-boot

typedef GENERATE(db_boot_spec_encode,db_boot_spec_decode)
struct {
  db_spec   spec;

  char      *kernel_name;
  char      *ramdisk_name;
  char      *kernel_hash_name;
  char      *ramdisk_hash_name;
  char      *security_context;
} db_boot_spec;


// -----------------------------------------------------------------------------
// db-server API

typedef GENERATE(db_build_request_encode,db_build_request_decode)
struct {
  size_t spec_len;
  uint8_t encoded_spec[spec_len];
  uint64_t host_store_id;     // Where to get images from
  uint64_t xenstore_id;       // Setup xenstore (or DB_CONFIG_NONE)
  uint64_t console_id;        // Setup console (or DB_CONFIG_NONE)
  uint64_t measured_build;    // Non-zero when measurement should be forwarded to the VTPM manager
} db_build_request;

// Information about built-in sevices (xen-store, console)
typedef struct {
  uint64_t mfn;       // Shared MFN (owned by guest)
  uint64_t port;      // Port       (in guest)
} service_info;

typedef GENERATE(db_build_response_encode,db_build_response_decode)
union {
  struct {
    uint64_t domain_id;
    uint64_t start_info_mfn;  // Location of start_info page (owned by guest)
    uint8_t kernel_hash[32];  // SHA256 size
    uint8_t ramdisk_hash[32]; // SHA256 size
    service_info console;
    service_info xenstore;
  } OK;

  uint64_t ERROR;
} db_build_response;
