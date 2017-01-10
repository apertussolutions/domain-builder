// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND


#include <dom_builder.h>
#include <smallOS/domctl.h>
#include <smallOS/mem.h>
#include <smallOS/printk.h>
#include <smallOS/events.h>
#include <smallOS/shutdown.h>
#include <smallOS/flask.h>
#include "image_loader.h"
#include <db-spec.h>
#include <cpio.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#define MAX_BOOT_VMS  32
#define MAX_IMAGE_Mbs 24
#define CONFIG_SUFFIX ".cfg.db"

static unsigned char uncompressed_kernel[MAX_IMAGE_Mbs * 1024 * 1024];
static uint8_t xs_start_info_page[PAGE_SIZE]  PAGE_ALIGNED RESET_VIRT;
static uint8_t start_info_page[PAGE_SIZE]     PAGE_ALIGNED RESET_VIRT;
static dom0_vga_console_info_t *hw_console;

typedef struct {
  domid_t       id;
  evtchn_port_t xs_port;
} dom_info_t;

static dom_info_t built_domains[MAX_BOOT_VMS];


#define LOG(args...) do { printk(args); } while(0)
#define ERR(args...) do { printk(args); } while(0)


/* Return 1 if input ends with suffix,
   Return 0 otherwise
 */
static int ends_with(const char *input, const char *suffix) {
  int input_len = strlen(input);
  int suffix_len = strlen(suffix);

  return suffix_len <= input_len
      && 0 == strcmp(&input[input_len - suffix_len], suffix);
}

/* Iterate through the files contained in the CPIO archive
   and find the next entry which ends with CONFIG_SUFFIX
 */
static int find_config( const void *cpio, unsigned long size
                      , const void **next, image *out) {
  if (cpio == NULL) return -ENOENT;

  for(;;) {
    const void *p = get_image(cpio, size, out);
    if (p == (void*)0)  return -ENOENT;
    if (p == (void*)-1) return -EINVAL;
    if (ends_with(out->name, CONFIG_SUFFIX)) {
      *next = p;
      return 0;
    }
    size -= p - cpio;
    cpio  = p;
  }
}

// Add the XenStore intro instructions to the command line of the control
// domain.
// Assumes that the start info page of the control domain is mapped in
// the array above. See also notes on the loop.
int add_intros(unsigned long how_many) {
  unsigned long i;
  char *cmdline = ((start_info_t*)(start_info_page))->cmd_line;
  size_t free;
  int needed;

  for (i = 0; i < MAX_GUEST_CMDLINE && cmdline[i] != 0; ++i);
  cmdline = &cmdline[i];
  free = MAX_GUEST_CMDLINE - i;

  // Don't add an empty intros argument
  if (how_many < 2) return 0;

  needed = snprintf(cmdline, free, " intros=");
  if (needed >= free) return -E2BIG;


  // Assuming that XenStore is the first, and control domain is the last.
  for (i = 1; i < how_many - 1; ++i) {
    free    -= needed;
    cmdline += needed;

    needed   = snprintf( cmdline, free, "%#llx,%#llx;"
                       , (uint64_t) built_domains[i].id
                       , (uint64_t) built_domains[i].xs_port
                       );
    if (needed >= free) return -E2BIG;
  }

  return 0;
}


void kernel_init(start_info_t* si) {
  int err;
  const void *search_cfg       = (const void*)si->mod_start;
  unsigned long search_size    = si->mod_len;
  unsigned long how_many_built = 0;
  int no_xs = 0;
  int max_start = 0;

  // Support two modes of booting:
  // startall - command line is "*"
  //   - All domains built with no xenstore
  //   - All VMs unpaused after building
  // sequenced - command line is an integer
  //   - The first domain is assumed to be xenstore
  //     - No xenstore information in start_info
  //     - Command line has " --event %d --master-domid %d" appended
  //   - All other domains have xenstore information set in their start page
  //   - The last domain built is assumed to be the control domain
  //     - Its xenstore information is added to xenstore command line
  //     - Command line has " intros=id,port;id,port;..." appended for
  //       translation to Xenstore introduction messages
  //   - Domains with IDs less than max_start are unpaused
  //
  // Since domain IDs are taken from the specification, the Xenstore domain
  // is not required to be among the automatically unpaused domains.

  if (si->cmd_line[0] == '*') {
    no_xs = 1;
  } else if (si->cmd_line[0] >= '0' && si->cmd_line[0] <= '9') {
    max_start = si->cmd_line[0] - '0';
  } else {
    max_start = 2;
  }

  // LOG("[DB-boot] Creating tablepage self-reference.\n");
  err = init_L4_loop();
  if (err != 0) {
     // ERR("Failed to create self-refernce(%d)\n", err);
     shutdown(SHUTDOWN_crash);
  }

  LOG("[DB-boot] Started.\n");
  hw_console = (void*)si + si->console.dom0.info_off;

  if (no_xs) {
    LOG("[DB-boot] Running in startall mode.\n");
  } else {
    LOG("[DB-boot] Running in handoff mode; starting domids <= %d.\n", max_start);
  }

  if (si->mod_len <= 0) {
    ERR("[DB-boot] Missing RAM disk!\n");
    shutdown(SHUTDOWN_crash);
  }

  while(how_many_built < MAX_BOOT_VMS) {
    image config;
    image kernel_image;
    image ramdisk_image;
    db_build_spec spec;
    db_boot_spec *params;
    const void *next_search = NULL;
    int err;
    int dbcf_err;

    // Look for a configuration file.
    err = find_config(search_cfg, search_size, &next_search, &config);
    if (err == -ENOENT) break;
    if (err == -EINVAL) {
      ERR("[DB-boot] Malformed RAM disk.\n");
      goto done;
    }
    search_size -= (next_search - search_cfg); search_cfg   = next_search;

    LOG("[DB-boot] Found config file: %s\n", config.name);

    params = (db_boot_spec*) config.bytes;
    dbcf_err = db_boot_spec_validate(&params, config.size);
    if (dbcf_err != 0) {
      if (dbcf_err < 0) {
        ERR("[DB-boot] Malformed configuration file (%d)\n",dbcf_err);
      } else {
        ERR("[DB-boot] Malformed configuration file (%s)\n",
                                        db_boot_spec_error_show(dbcf_err));
      }
      goto done;
    }
    spec.basics = &params->spec;


    // Find kernel image
    err = find_file_cpio( (const void*) si->mod_start, si->mod_len
                        , params->kernel_name, &kernel_image);
    if (err != 0) {
      ERR("[DB-boot] Kernel \"%s\" not found\n", params->kernel_name);
      goto done;
    }

    // Decompress image, if necessary
    if (test_elf(kernel_image.bytes, kernel_image.size) == 0) {
      LOG("[DB-boot] Found ELF\n");
      spec.image      = kernel_image.bytes;
      spec.image_size = kernel_image.size;
    } else if (check_header(kernel_image.bytes, kernel_image.size) == 0) {
      LOG("[DB-boot] Found bzImage\n");
      spec.image_size = extract_bzimage( kernel_image.bytes
                                       , kernel_image.size
                                       , uncompressed_kernel
                                       , sizeof(uncompressed_kernel));
      if (spec.image_size == (uint64_t) -1) {
        ERR("[DB-boot] Failed to extract bzImage\n");
        goto done;
      }
      LOG("[DB-boot] Extracted %d bytes\n", spec.image_size);
      spec.image = uncompressed_kernel;
    } else {
      ERR("[DB-boot] Bad kernel image\n");
      goto done;
    }

    // Find RAM disk, if any
    if (params->ramdisk_name != NULL) {
      err = find_file_cpio( (const void*) si->mod_start, si->mod_len
                          , params->ramdisk_name, &ramdisk_image );
      if (err != 0) {
        ERR("[DB-boot] Ramdisk \"%s\" not found\n", params->ramdisk_name);
        goto done;
      }

      spec.ram_disk      = ramdisk_image.bytes;
      spec.ram_disk_size = ramdisk_image.size;
    } else {
      spec.ram_disk      = NULL;
      spec.ram_disk_size = 0;
    }

    // Initial VMs do not have a console.
    spec.console.domid  = ~0ULL;
    // The first domain to be built is assumed to be the XenStore.
    spec.xenstore.domid = (how_many_built == 0 || no_xs) ?
                                                ~0ULL : built_domains[0].id;

    if (params->security_context == NULL) {
      ERR("[DB-boot] Security context not specified\n");
      goto done;
    }

    uint32_t sid;
    err = flask_context_to_sid(strlen(params->security_context), params->security_context, &sid);
    if (err != 0) {
      ERR("[DB-boot] Failed to compute SID: %d\n", err);
      goto done;
    }

    // Now, build a VM
    LOG("[DB-boot] Setting up domain with %lluKB of memory.\n", spec.basics->allocate_kbs);
    err = create_vm( how_many_built == 0 ? xs_start_info_page
                                         : start_info_page
                   , &spec, hw_console, sid
                   );

    if (err < 0) {
      ERR("[DB-boot] Failed to setup domain: %d\n", err);
      goto done;
    }

    // xenstored needs access to this VIRQ
    if (how_many_built == 0)
      set_virq_handler(spec.basics->domid, VIRQ_DOM_EXC);

    // Record interesting information about the built-domain.
    built_domains[how_many_built].id = spec.basics->domid;

    if (! (how_many_built == 0 || no_xs)) {
      start_info_t *guest_si                = (void*)&start_info_page;
      built_domains[how_many_built].xs_port = guest_si->store_evtchn;
    }

    ++how_many_built;
  }

  if (no_xs) {

  // Mode 1: Just start everyone (the "old" behavior)
    unsigned long i;
    for (i = 0; i < how_many_built; ++i) {
      domid_t d = built_domains[i].id;
      LOG("[DB-boot] Unpausing domain with id %d...\n", d);
      err = unpause_domain(d);
      if (err < 0) ERR("Failed to unpause domain %d.\n", d);
    }

  // Mode 2: Add introduction info to control domain, then start it.
  } else {
    // Control domain is assumed to be last.
    unsigned long ctrl_ix = how_many_built - 1;
    unsigned long i;

    if (how_many_built < 2) {
      ERR("[DB-boot] Control domain handoff mode requires at least two domains, for the old mode use the 'startall' flag\n");
      goto done;
    }

    err = add_intros(how_many_built);
    if (err != 0) {
      ERR("[DB-boot] Failed to extend command line arguments (%d)\n", err);
      goto done;
    }

    { start_info_t *xs_si = (void*)&xs_start_info_page;
    // Let the XenStore know how to speak to the control domain.
      char *cmdline = xs_si->cmd_line;
      int len = MAX_GUEST_CMDLINE;
      while (*cmdline) {
        cmdline++;
        len--;
      }
      snprintf(cmdline, len, " --event %d --master-domid %d",
               (int)built_domains[ctrl_ix].xs_port,
               (int)built_domains[ctrl_ix].id);
    }

    // Now start the domains
    for (i = 0; i < how_many_built; ++i) {
      domid_t d = built_domains[i].id;
      if (d > max_start)
        continue;
      LOG("[DB-boot] Unpausing domain with id %d...\n", d);
      err = unpause_domain(d);
      if (err < 0) ERR("Failed to unpause domain %d.\n", d);
    }
  }

done:
  LOG("[DB-boot] Domain builder complete.\n");
  {
     struct sched_shutdown down = { .reason = 0 };
     HYPERVISOR_sched_op(SCHEDOP_shutdown, &down);
  }
  ERR("[DB-boot] Shutdown failed, blocking\n");
  while(1) {
    block();
    LOG("[DB-boot] unblocked");
  }
}

void kernel_loop(void) {}


