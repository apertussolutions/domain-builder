#include <smallOS/printk.h>
#include <smallOS/shutdown.h>
#include <smallOS/chan.h>
#include <smallOS/shared_info.h>
#include <smallOS/grant_table.h>
#include <smallOS/events.h>
#include <smallOS/evtchn.h>
#include <smallOS/mem.h>
#include <smallOS/domctl.h>
#include <smallOS/xs.h>
#include <errno.h>
#include <db-spec.h>
#include <db-api/decode.h>
#include <string.h>
#include <cpio.h>
#include <stdio.h>
#include <stdlib.h>
#include <sha256.h>
#include <xen/io/xs_wire.h>

#include <control-domain-api/decode.h>
#include <control-domain-api/encode.h>
#include "driverprep.h"

#include <control-domain.h>
#include <control-domain-schema/decode.h>

#include "log.h"
#include "cmdlines.h"

#define MAX_DOMAINS 32
#define INTRODUCE_DIR "/introduce"

#define GT_FRAMES   1
#define GT_REF_NUM  (GT_FRAMES * PAGE_SIZE / sizeof(grant_entry_v2_t))
static grant_entry_v2_t grant_table[GT_REF_NUM] PAGE_ALIGNED;

// Connection to db-server
uint8_t db_mem[PAGE_SIZE] PAGE_ALIGNED;
static chan_t  db_chan;

// Connection to SVP Xenstore
static uint8_t svp_xenstore_shared[PAGE_SIZE] PAGE_ALIGNED NO_RESET_VIRT;
xs_chan_t svp_xs_chan = { .ring = (void*)svp_xenstore_shared };

// The platform console
static domid_t console_domain = -1;
domid_t vtpm_domain = -1;

// The platform members
static int platform_members_created = 0;
static platform_member_info platform_members[MAX_DOMAINS];

// This is used to handle requests.
static char command_response_shared[PAGE_SIZE] PAGE_ALIGNED NO_RESET_VIRT;
static char command_response[PAGE_SIZE] PAGE_ALIGNED;

// A memory area where we can map the guest's start info page
// in case we need to configure it some more (e.g., add to its command line).
static uint8_t guest_start_info[PAGE_SIZE] PAGE_ALIGNED NO_RESET_VIRT;

// Currently, this is used to store the names of dynamically built VMs.
// "Dynamically", refers to VMs built after the platform has been built.
// For memebers of the platform we just keep pointers to their names in the schema.
uint8_t global_heap_space[4 * PAGE_SIZE] PAGE_ALIGNED NO_RESET;
heap_t global_heap;

cmdline_opts_t opts =
  { .svp_mode           = 0
  , .db_dom             = -1
  , .host_store_domain  = -1
  , .drivers_dom        = -1
  , .xenstore_domain    = -1
  , .ctrl_dom           = -1
  , .vtpm_manager_dom   = -1

  , .db_dm              = NULL
  , .host_store_dm      = NULL
  , .drivers_dm         = NULL
  , .xenstore_dm        = NULL
  , .ctrl_dm            = NULL
  , .vtpm_manager_dm    = NULL

  , .intro_num          = 0
  };



// Cached data.  Should not change during the life time of the control domain.
static start_info_t *cd_start_info;
static void  *cd_ram_disk;
static size_t cd_ram_disk_size;

static int boot_platform(const control_domain_schema* schema);

static start_info_t * map_guest_start_info(db_build_response new_dom);
static int unmap_guest_start_info(void);

static int introduce_console(domid_t new_dom, service_info *con);
static int introduce_xenstore(domid_t new_dom, service_info *con);

static void wait_for_host_storage(void);


static void event_loop(void) __attribute__((noreturn));
static void setup_platform(void);

int kernel(start_info_t* start_info);

/* Add new domid to the list of created domains
   Return 0 on success
   Return negative error number on failure
 */
static
int add_platform_member(domid_t domid, char *name) {
  if (platform_members_created >= MAX_DOMAINS) {
    ERR("[CTL] Too many VMs in platform (current limit is %d)\n", MAX_DOMAINS);
    return -E2BIG;
  }
  platform_members[platform_members_created].domid = domid;
  platform_members[platform_members_created].name  = name;
  ++platform_members_created;
  return 0;
}

// Let the console service know about a new VM which is being started.
static
int introduce_console( domid_t new_dom
                     , service_info *con) {
  int tres;
  do {
      uint32_t txid;
      xs_start_transaction(&svp_xs_chan, &txid);
  
      xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL,
        "/local/domain/%lu/console/ring-ref", (unsigned long) new_dom,
        "%lu", (unsigned long) con->mfn,
        NULL);
      xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL,
        "/local/domain/%lu/console/port", (unsigned long) new_dom,
        "%lu", (unsigned long) con->port,
        NULL);
      xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL,
        "/local/domain/%lu/console/ring-ref", (unsigned long) new_dom,
        "n%lu", (unsigned long) new_dom,
        NULL);
      xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL,
        "/local/domain/%lu/console/port", (unsigned long) new_dom,
        "n%lu", (unsigned long) new_dom,
        NULL);
      xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL,
        "/local/domain/%lu/console", (unsigned long) new_dom,
        "n%lu", (unsigned long) 0,
        NULL);
    tres = xs_end_transaction(&svp_xs_chan, txid, 1);
  } while (tres == -EAGAIN);

  return 0;
}



static
start_info_t * map_guest_start_info(db_build_response new_dom)
{
  start_info_t *start_info;
  int err;

  LOG("[CTL] Mapping start info page\n");
  err = map_foreign_page(new_dom.u.OK.domain_id, guest_start_info,
                    new_dom.u.OK.start_info_mfn, _PAGE_PRESENT | _PAGE_RW);
  if (err != 0) {
    ERR("[CTL] Failed to map start info page (%d)\n", err);
    return NULL;
  }
  start_info = (start_info_t*) guest_start_info;

  return start_info;
}

static
int unmap_guest_start_info(void)
{
  start_info_t *start_info = (start_info_t*) guest_start_info;
  int err;

  LOG("[CTL] Unmapping start info page\n");
  err = unmap_foreign_page(DOMID_SELF, start_info);
  if (err != 0) {
    ERR("[CTL] Failed to unmap start info page (%d)\n", err);
    return err;
  }
  return 0;
}

static
int introduce_xenstore(domid_t new_dom, service_info *con) {

  uint32_t txid;
  int tres;

  xs_introduce(&svp_xs_chan, new_dom, con->mfn, con->port);

  do {
    xs_start_transaction(&svp_xs_chan, &txid);
  
    xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL,
        "/local/domain/%lu/domid", (unsigned long) new_dom,
        "%lu", (unsigned long) new_dom,
        NULL);
    xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL,
        "/local/domain/%lu/domid", (unsigned long) new_dom,
        "n%lu", 0,
        "r%lu", (unsigned long) new_dom,
        NULL);
  
    xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL,
       "/local/domain/%lu/vm", (unsigned long) new_dom,
       "/vm/%lu", (unsigned long) new_dom,
       NULL);
    xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL,
       "/local/domain/%lu/vm", (unsigned long) new_dom,
       "n%lu", 0,
       "r%lu", (unsigned long) new_dom,
       NULL);
  
    xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL,
       "/local/domain/%lu", (unsigned long) new_dom,
       "n%lu", 0,
       "r%lu", (unsigned long)new_dom,
       NULL);

    tres = xs_end_transaction(&svp_xs_chan, txid, 1);
  } while (tres == -EAGAIN);

  return 0;
}


static
int setup_introduce_dir(void) {
  int err;

  // Setup /introduce to control who can introduce to XenStore
  err = xs_command( &svp_xs_chan, XS_MKDIR, NO_TRANSACTION, NULL, NULL, INTRODUCE_DIR, NULL);
  if (err != 0) {
    ERR("[CTL] Failed to create " INTRODUCE_DIR "\n");
    //return err;
  }

  return 0;
}


static int intros_introduce(intro_t *info) {
  int err;
  service_info con = { .mfn = info->mfn, .port = info->port };

  LOG("[CTL] Introducing domain %u (%#lx, %u) to XenStore\n",
     (unsigned) info->domid, (unsigned long) info->mfn, (unsigned) info->port);

  err = introduce_xenstore(info->domid, &con);
  if (err < 0) {
    return err;
  }

#if 0
  LOG ("[CTL] Unpausing %u\n", (unsigned) info->domid);
  err = unpause_domain(info->domid);
  if (err < 0) {
    ERR ("[CTL] Failed to unpause domain %d\n", info->domid);
    return err;
  }
#endif

  return 0;
}

// Note that both SVP and UVP control domains have a link to the
// SVP Xen store so that they can contact the various hardware managers.
static
int setup_svp_xenstore(void) {
  int err;

  err = map_foreign_page(DOMID_SELF, svp_xenstore_shared,
                    cd_start_info->store_mfn, _PAGE_PRESENT | _PAGE_RW);
  if (err != 0) return err;

  svp_xs_chan.port = cd_start_info->store_evtchn;

  return 0;
}


// XXX: If this fails, then we should probably shutdown and destroy the
// platform members...
static int boot_svp(void) {
  int err;
  unsigned long i;

  err = add_platform_member(opts.xenstore_domain, "xenstore");
  if (err != 0) return err;

  LOG ("[CTL] Unpause the XenStore domain\n");
  err = unpause_domain(opts.xenstore_domain);
  if (err < 0) {
    ERR ("[CTL] Failed to unpause the XenStore domain\n");
    return err;
  }

  LOG ("[CTL] Wait for startup\n");
  err = receive_event(0, 1, &svp_xs_chan.port);
  if (err < 0) {
    ERR ("[CTL] Failed to wait for XenStore startup\n");
    return err;
  }

  err = setup_introduce_dir();
  if (err != 0) return err;

  for (i = 0; i < opts.intro_num; ++i) {
    char *name;
    int ishvm;
    domid_t dom = opts.intros[i].domid;

    if (dom == opts.db_dom)            {
      name = "domain builder";
      if (opts.db_dm) {
        prepare_drivers(dom, cd_ram_disk, cd_ram_disk_size, opts.db_dm, &ishvm);
      }
    } else if (dom == opts.host_store_domain) {
      name = "host store";
      if (opts.host_store_dm) {
        prepare_drivers(dom, cd_ram_disk, cd_ram_disk_size, opts.host_store_dm, &ishvm);
      }
    } else if (dom == opts.drivers_dom) {
      name = "drivers";
      if (opts.drivers_dm) {
        prepare_drivers(dom, cd_ram_disk, cd_ram_disk_size, opts.drivers_dm, &ishvm);
      }
    } else if (dom == opts.ctrl_dom) {
      name = "control";
      if (opts.ctrl_dm) {
        prepare_drivers(dom, cd_ram_disk, cd_ram_disk_size, opts.ctrl_dm, &ishvm);
      }
    } else if (dom == opts.vtpm_manager_dom) {
      name = "vtpmmgr";
      if (opts.vtpm_manager_dm) {
        prepare_drivers(dom, cd_ram_disk, cd_ram_disk_size, opts.vtpm_manager_dm, &ishvm);
      }
    } else {
      name = "(unknown)";
    }

    LOG("[CTL] running intros for %d\n", (int) dom);
    err = intros_introduce(&opts.intros[i]);
    if (err < 0) {
      return err;
    }

    /* XXX temporary linerization of boot: only unpause hardware domain */
    if (dom == opts.host_store_domain) {
	  LOG ("[CTL] Unpausing %u\n", (unsigned) dom);
	  err = unpause_domain(dom);
	  if (err < 0) {
		ERR ("[CTL] Failed to unpause domain %d\n", dom);
		return err;
	  }
	}

    err = add_platform_member(opts.intros[i].domid, name);
    if (err != 0) return err;
  }

  wait_for_host_storage();

  return 0;
}

/* The control domain needs to wait for the host storage domain to be ready
   before the db-server will be able to handle any request. The host storage
   domain must be configured to write the string "ready" to the status key
   in the xenstore: "/local/domain/$id/status"

   This function will poll the xenstore out of convenience. It would be
   appropriate to install a watch.
 */
static
void wait_for_host_storage(void) {

  char resp[10] = {0};

  do {
    int res;
    size_t rlen = sizeof(resp);

    receive_event(1000000000, 0, NULL);

    res = xs_command(&svp_xs_chan, XS_READ, NO_TRANSACTION,
      resp, &rlen,
      "/local/domain/%lu/status", (unsigned long) opts.host_store_domain,
      NULL);
    if (res < 0) { continue; }

  } while (strncmp(resp, "ready", 5) != 0);
}

int kernel(start_info_t* start_info) {
  int err = (unsigned long)start_info;

  cd_start_info    = start_info;
  cd_ram_disk      = (void*)cd_start_info->mod_start;
  cd_ram_disk_size = cd_start_info->mod_len;

  err = init_L4_loop();
  if (err != 0) {
    ERR ("[CTL] Failed to setup L4 loop (%d)\n", err);
    shutdown(SHUTDOWN_crash);
  }

  init_heap(&global_heap, global_heap_space, sizeof(global_heap_space));

  err = init_shared_info(start_info);
  if (err != 0) {
    ERR ("[CTL] Failed to setup shared info (%d)\n", err);
    shutdown(SHUTDOWN_crash);
  }

  err = setup_grant_table(grant_table, GT_FRAMES);
  if (err != 0) {
    ERR ("[CTL] Failed to setup grant_table (%d)\n", err);
    shutdown(SHUTDOWN_crash);
  }

  // Both SVP and UVP control domains have a connection to
  // the SVP xenstore, so that they can make introductions to manages.
  err = setup_svp_xenstore();
  if (err != 0) {
    ERR ("[CTL] Failed to setup SVP Xenstore (%d)\n", err);
    shutdown(SHUTDOWN_crash);
  }

  // Note that svp mode will not be set until this command is processed!
  err = load_cmdline(start_info->cmd_line, &opts);
  if (err != 0) {
    ERR ("[CTL] Failed to load arguments (%d)\n", err);
    shutdown(SHUTDOWN_crash);
  }

  setup_platform();
  event_loop();
}

/* This function creates the initial platform according to the provided
   schema as specified on the commandline and provided in the ramdisk */
static
void setup_platform(void) {
  // The command line specifies the name of the platform schema file
  image schema_image = {0};

  int err = find_file_cpio( cd_ram_disk, cd_ram_disk_size
                      , opts.schema_filename, &schema_image
                      );
  if (err != 0) {
    ERR ("[CTL] Could not load schema file (%d)\n", err);
    shutdown(SHUTDOWN_crash);
  }

  control_domain_schema * schema_ptr = schema_image.bytes;

  err = cd_schema_decode((void**)&schema_ptr, schema_image.size);
  if (err != 0) {
    ERR ("[CTL] Schema validation failed (%d)\n", err);
    shutdown(SHUTDOWN_crash);
  }

  err = boot_platform(schema_ptr);
  if (err != 0) {
    ERR ("[CTL] Aborting due to failure to build schema. (%d)\n", err);
    //shutdown(SHUTDOWN_crash);
  }
}

static
void event_loop(void) {
  // The main event loop.
  // Contents removed due to lack of IVC support in upstream hypervisor
  for(;;)
    block();
}


static
int boot_platform(const control_domain_schema * schema) {
  size_t i;
  int err;
  int console_ix, vtpm_ix;
  const char * console_name = "console";
  const char * vtpm_name = "vtpm";

  // If we are the SVP, then we need to start the very basic
  // services first (xenstore, db-server, host-storage).
  if (opts.svp_mode) {
    LOG ("[CTL] Entering SVP mode\n");
    err = boot_svp();
    if (err != 0) {
      ERR ("[CTL] Failed to start SVP mode (%d)\n", err);
      shutdown(SHUTDOWN_crash);
    }
  } else {
    ERR ("[CTL] Non-SVP mode requires IVC support, not available\n");
  }

  return 0;
}

