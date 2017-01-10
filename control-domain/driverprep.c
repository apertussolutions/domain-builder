#include <stdint.h>
#include <stdio.h>
#include <errno.h>
#include <cpio.h>
#include <smallOS/mem.h>
#include <smallOS/printk.h>
#include <xen/hvm/hvm_op.h>
#include <xen/hvm/params.h>
#include <db-spec.h>

#include "cmdlines.h"
#include "driverprep.h"
#include "log.h"

static int lookup_dm_attribute(char *file, size_t n, const char *key, char *value, size_t valsize);
static int prepare_vbds(domid_t domid, char *file, size_t n);
static int prepare_vbd(domid_t domid, unsigned drivenum, char *file, size_t n);
static int prepare_tpms(domid_t domid, char * dmfile, size_t dmlen);
static int prepare_generic_device(uint32_t txid, domid_t backid, domid_t frontid, const char *devtype, unsigned devnum);
static int set_hvm_param(domid_t dom, uint32_t param, uint64_t val);

extern xs_chan_t svp_xs_chan;
extern cmdline_opts_t opts;
extern domid_t vtpm_domain;

static
int determine_ishvm(char *dmfile, size_t dmlen) {

  char value[2] = {0};
  int err = lookup_dm_attribute(dmfile, dmlen, "HVM", value, sizeof(value));
  return err == 0 && strcmp(value, "1") == 0;

}

int prepare_drivers(domid_t domid, void *cpio_file, size_t cpio_size, const char* filename,
                    int *is_hvm_out)
{
  int err;
  image img;

  LOG("[CTL] Loading driver configuration for dom %d\n", (int)domid);

  err = find_file_cpio(cpio_file, cpio_size, filename, &img);
  if (err < 0) {
    ERR("[CTL] Failed to load DM file %s (%d)\n", filename, err);
    return err;
  }


  int ishvm = determine_ishvm(img.bytes, img.size);
  *is_hvm_out = ishvm;

  if (ishvm) {
    set_hvm_param(domid, HVM_PARAM_DM_DOMAIN, opts.drivers_dom);
  } else {
    prepare_vbds(domid, img.bytes, img.size);
    err = prepare_tpms(domid, img.bytes, img.size);
    if (err != 0) { return err; }
  }

  return 0;
}

static
int wrapped_prepare_generic_device(domid_t back, domid_t front, char *dev, int handle) {
  int err;

  do {
    uint32_t txid;

    err = xs_start_transaction(&svp_xs_chan, &txid);
    if (err != 0) { return err; }

    err = prepare_generic_device(txid, back, front, dev, handle);
    if (err != 0) {
      xs_end_transaction(&svp_xs_chan, txid, 0);
      return err;
    }

    xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL,
	"/local/domain/%lu/backend/%s/%lu/%u/uuid", (unsigned long)back, dev, (unsigned long)front, handle,
	"ba7254bd-10a6-4273-bf0c-fb4d%08lx", (unsigned long) front, NULL);

    err = xs_end_transaction(&svp_xs_chan, txid, 1);
  } while (err == -EAGAIN);

  if (err != 0) { return err; }

  return 0;
}

static
int prepare_tpms(domid_t domid, char * dmfile, size_t dmlen) {
    char value[5];
    int err;

    err = lookup_dm_attribute(dmfile, dmlen, "TPM", value, sizeof(value));
    if (err == 0) {
       LOG("Attaching pretend TPM device\n");

       err = wrapped_prepare_generic_device(opts.drivers_dom, domid, "vtpm", 0);
       if (err != 0) {
         ERR("Failed with error: %d\n", err);
       }
    }

    err = lookup_dm_attribute(dmfile, dmlen, "VTPM", value, sizeof(value));
    if (err == 0) {
       if (vtpm_domain == -1) {
         ERR("No VTPM domain known\n");
       } else {
         LOG("Attaching to vTPM\n");

         err = wrapped_prepare_generic_device(vtpm_domain, domid, "vtpm", 0);
         if (err != 0) {
           ERR("Failed with error: %d\n", err);
         }
      }
    }

    // domains requiring a TPM manager are connected to the domain specified
    // on the commandline with tpm=<domid>
    // SVP platform specific
    err = lookup_dm_attribute(dmfile, dmlen, "TPMMGR", value, sizeof(value));
    if (err == 0) {
       if (opts.vtpm_manager_dom == -1) {
         ERR("No VTPM Manager domain known\n");
       } else {
         LOG("Attaching to TPM Manager\n");

         err = wrapped_prepare_generic_device(opts.vtpm_manager_dom, domid, "vtpm", 0);
         if (err != 0) {
           ERR("Failed with error: %d\n", err);
         }
      }
    }

    return 0;
}

static
int lookup_dm_attribute(char *file, size_t n, const char *key, char *value, size_t valsize) {

  size_t i = 0;
  size_t j;
  size_t keylen = strlen(key);

  while (i + keylen < n) {
    // Check if current line matches requested key
    if (0 == strncmp(key, &file[i], keylen) && file[i + keylen] == '=' && file[i + keylen + 1] == '"') {

      // Skip the key and = and " characters
      i += keylen + 2;

      // Copy the value into the output buffer
      for (j = 0; i < n && file[i] != '"' && file[i] != '\0' && j < valsize; i++, j++) {
        value[j] = file[i];
      }

      // Add null terminator if it fits
      if (j < valsize) { value[j] = '\0'; } else { return 2; }

      // Ensure that the argument was a fully quoted string
      if (i < n && file[i] == '"') { return 0; } else { return 1; }
    }

    // Advance to the beginning of the next line
    while(i < n && file[i] != '\n' && file[i] != '\0') { i++; }
    if (i < n && file[i] == '\n') { i++; }
  }
  return 3;
}

/* Most (if not all) front-end/back-end drivers in Xen rendezvous through
   a stanard pattern of key/value pairs in the XenStore. This function
   creates those pairs. Afterward additionally settings can be stored in
   the resulting tree.
 */
static int prepare_generic_device(uint32_t txid, domid_t backid, domid_t frontid, const char *devtype, unsigned devnum) {

  char backfmt[60] = {0};
  char frontfmt[60] = {0};

  snprintf(backfmt, sizeof(backfmt),   "/local/domain/%lu/backend/%s/%lu/%u%%s", (unsigned long)backid, devtype, (unsigned long)frontid, devnum);
  snprintf(frontfmt, sizeof(frontfmt), "/local/domain/%lu/device/%s/%u%%s",      (unsigned long)frontid, devtype, devnum);

  xs_command(&svp_xs_chan, XS_WRITE,     txid, NULL, NULL, frontfmt, "/backend"   , backfmt, "", NULL);
  xs_command(&svp_xs_chan, XS_WRITE,     txid, NULL, NULL, frontfmt, "/backend-id", "%lu", (unsigned long)backid, NULL);
  xs_command(&svp_xs_chan, XS_WRITE,     txid, NULL, NULL, frontfmt, "/state"     , "1", NULL);
  xs_command(&svp_xs_chan, XS_WRITE,     txid, NULL, NULL, frontfmt, "/handle"    , "%u", devnum, NULL);

  xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL, frontfmt, "/backend"   , "n%lu", (unsigned long)frontid, "r%lu", (unsigned long)backid, NULL);
  xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL, frontfmt, "/backend-id", "n%lu", (unsigned long)frontid, "r%lu", (unsigned long)backid, NULL);
  xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL, frontfmt, "/state"     , "n%lu", (unsigned long)frontid, "r%lu", (unsigned long)backid, NULL);
  xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL, frontfmt, "/handle"     , "n%lu", (unsigned long)frontid, "r%lu", (unsigned long)backid, NULL);
  xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL, frontfmt, ""           , "n%lu", (unsigned long)frontid, "r%lu", (unsigned long)backid, NULL);

  xs_command(&svp_xs_chan, XS_WRITE,     txid, NULL, NULL, backfmt, "/frontend"   , frontfmt, "", NULL);
  xs_command(&svp_xs_chan, XS_WRITE,     txid, NULL, NULL, backfmt, "/frontend-id", "%lu", (unsigned long)frontid, NULL);
  xs_command(&svp_xs_chan, XS_WRITE,     txid, NULL, NULL, backfmt, "/state"      , "1" , NULL);
  xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL, backfmt, "/state"      , "n%lu", (unsigned long)backid, "r%lu", (unsigned long) frontid, NULL);
  xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL, backfmt, "/frontend"      , "n%lu", (unsigned long)backid, "r%lu", (unsigned long) frontid, NULL);
  xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL, backfmt, "/frontend-id"      , "n%lu", (unsigned long)backid, "r%lu", (unsigned long) frontid, NULL);
  xs_command(&svp_xs_chan, XS_SET_PERMS, txid, NULL, NULL, backfmt, ""      , "n%lu", (unsigned long)backid, "r%lu", (unsigned long) frontid, NULL);

  return 0;
}

/* Create the entries necesary for a given VBD given a configuration drive
   number and a device model configuration file.

   Return 0 on success
 */
static
int prepare_vbd(domid_t domid, unsigned drive_num, char *file, size_t n) {
  char key[30];
  char dev[10];
  char bootable[2];
  char mode[5];
  char type[10];
  char devicetype[10];
  char path[100];
  char virtualdevice[10];
  int res;

  snprintf(key, sizeof(key), "VBD_DEV[%u]", drive_num);
  res = lookup_dm_attribute(file, n, key, dev, sizeof(dev));
  if (res != 0) return res;

  snprintf(key, sizeof(key), "VBD_TYPE[%u]", drive_num);
  res = lookup_dm_attribute(file, n, key, type, sizeof(type));
  if (res != 0) return res;

  snprintf(key, sizeof(key), "VBD_MODE[%u]", drive_num);
  res = lookup_dm_attribute(file, n, key, mode, sizeof(mode));
  if (res != 0) return res;

  snprintf(key, sizeof(key), "VBD_PARAMS[%u]", drive_num);
  res = lookup_dm_attribute(file, n, key, path, sizeof(path));
  if (res != 0) return res;

  snprintf(key, sizeof(key), "VBD_BOOTABLE[%u]", drive_num);
  res = lookup_dm_attribute(file, n, key, bootable, sizeof(bootable));
  if (res != 0) return res;

  snprintf(key, sizeof(key), "VBD_DEVICE_TYPE[%u]", drive_num);
  res = lookup_dm_attribute(file, n, key, devicetype, sizeof(devicetype));
  if (res != 0) return res;

  snprintf(key, sizeof(key), "VBD_VIRTUAL_DEVICE[%u]", drive_num);
  res = lookup_dm_attribute(file, n, key, virtualdevice, sizeof(virtualdevice));
  if (res != 0) return res;

  LOG("[CTL] Successfully parsed VBD entries\n");

  char backfmt[60], frontfmt[60];
  snprintf(backfmt, sizeof(backfmt), "/local/domain/%lu/backend/vbd/%lu/%s/%%s",
             (unsigned long) opts.drivers_dom, (unsigned long) domid, virtualdevice);
  snprintf(frontfmt, sizeof(frontfmt), "/local/domain/%lu/device/vbd/%s/%%s",
             (unsigned long)domid, virtualdevice);

  do {
    uint32_t txid;
    xs_start_transaction(&svp_xs_chan, &txid);
    LOG("[CTL] Starting transaction %d %d\n", res, txid);

    prepare_generic_device(txid, opts.drivers_dom, domid, "vbd", atoi(virtualdevice));

    xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL, backfmt, "bootable", "%s", bootable, NULL);
    xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL, backfmt, "dev"     , "%s", dev, NULL);
    xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL, backfmt, "type"    , "%s", type, NULL);
    xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL, backfmt, "info"    , "0", NULL);
    xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL, backfmt, "mode"    , "%s", mode, NULL);
    xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL, backfmt, "sector-size", "512", NULL);
    xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL, backfmt, "params", "%s", path, NULL);

    xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL, frontfmt, "virtual-device", "%s", virtualdevice, NULL);
    xs_command(&svp_xs_chan, XS_WRITE, txid, NULL, NULL, frontfmt, "device-type", "%s", devicetype, NULL);

    res = xs_end_transaction(&svp_xs_chan, txid, 1);
  } while (res == -EAGAIN);
  return res;
}

/* Establish the connection for the given domain for all of the
   VBDs listed in the given device model configuration file.

   Return 0 on success
 */
static
int prepare_vbds(domid_t domid, char *file, size_t n) {
  char value[16];
  int res;
  unsigned i;
  unsigned drives;

  res = lookup_dm_attribute(file, n, "VBD_NUM", value, sizeof(value));
  if (res != 0) { LOG("No drives\n"); return res; }

  drives = atoi(value);

  for (i = 0; i < drives; i++) {
    res = prepare_vbd(domid, i+1, file, n); // DM file indexes start at 1
    if (res != 0) return res;
  }

  return 0;
}

/* This is a simple wrapper around the hyper-call for setting a
   Xen parameter on an HVM domain.
 */
static
int set_hvm_param(domid_t dom, uint32_t param, uint64_t val) {
  xen_hvm_param_t cmd =
    { .domid = dom
    , .index = param
    , .value = val
    };
  return HYPERVISOR_hvm_op(HVMOP_set_param, &cmd);
}
