// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND

/*
 * This is the main entry point for the library.
 * It defines a single function called "create_vm", which creates and
 * initializes the initial state for a new vertual machine.
 *
 */

#include <errno.h>
#include <dom_builder.h>
#include <smallOS/domctl.h>
#include <smallOS/flask.h>
#include <string.h>
#include "db.h"


// NOTE: For structures that do not fill a whole page, is there an
// expectation that we pad the rest with 0?  At the moment, we do not.
// We may want to do it, to avoid accidental leackage of information.


// Used as a "window" in the guest's virtual space by various
// domain building functions.
static uint8_t view[PAGE_SIZE] RESET_VIRT PAGE_ALIGNED;

#define create_vm_pv(ARCH)            POLY(create_vm_pv,ARCH)
#define setup_service(ARCH)           POLY(setup_service,ARCH)


#define ARCH x86_64
#define WX 64
#include "dom_builder.generic.c"
#undef WX
#undef ARCH

#define ARCH x86_32pae
#define WX 32
#include "dom_builder.generic.c"
#undef WX
#undef ARCH


static
int new_domain
  ( uint64_t domid
  , uint32_t sid
  , int is_hvm
  , int is_priv
  , domid_t *dom
  ) {
  int err;
  uint32_t flags = 0;
  domid_t did;
  char context[50];
  uint32_t size32 = sizeof(context);
  err = flask_sid_to_context(&size32, context, sid);
  DB_DEBUG("[DB] Checking context for SSID: %s\n", context);

  did = (domid_t) domid;
  if ((uint64_t)did != did) {
    DB_ERROR("Invalid domin ID request (%llu)\n", domid);
    return -EINVAL;
  }

  // Create a new domain
  if (is_hvm) flags |= XEN_DOMCTL_CDF_hvm_guest;

  err = create_domain(did, sid, flags, dom);
  if (err != 0) {
    DB_ERROR ("Failed to create domain (%d)\n", err);
    return err;
  }
  DB_DEBUG("[DB] Created domain (domid = %d)\n", *dom);

  return 0;
}



int create_vm
  ( void *start_info_area    // XXX: Perhaps make more type checked
  , db_build_spec *spec
  , dom0_vga_console_info_t *hw_console
  , uint32_t sid
  ) {

  dom_info_t info;
  int err, err1;

  memset(&info, 0, sizeof(dom_info_t));

  // Most of "info" is initialize here
  info.page_num = number_of_units(spec->basics->allocate_kbs * 1024, PAGE_SIZE);
  info.cpu_num = spec->basics->max_vcpus;

  DB_DEBUG("[DB] Parsing ELF:\n");
  err = load_elf( &info
                , spec->basics->vm.tag == hvm
                , spec->image_size
                , spec->image
                , view
                );
  if (err != 0) return err;
  DB_DEBUG("[DB] Parsing ELF completed.\n");


  // Info gets fully initialized here.
  err = new_domain( spec->basics->domid
                  , sid
                  , spec->basics->vm.tag == hvm
                  , spec->basics->dom_flags & 1
                  , &info.dom);
  if (err != 0) return err;
  // NOTE: After this point we need to remember to destroy the domain
  // if something goes wrong.

  DB_DEBUG("[DB] Setting max vcpus to %u.\n", info.cpu_num);
  err = set_max_vcpus(info.dom, info.cpu_num);
  if (err != 0) goto error_with_dom;

  { // We add an extra megabyte to the max mem for extra metadata pages
    // (e.g., "special" pages in HVM)
    size_t max_mem = spec->basics->max_kbs + 1024;

    DB_DEBUG("[DB] Setting up max memory to %d KB.\n", max_mem);
    err = set_max_mem(info.dom, max_mem);
    if (err != 0) goto error_with_dom;
  }

  DB_DEBUG("[DB] Setting up CPUID policy.\n", spec->basics->allocate_kbs);

  err = xc_cpuid_apply_policy
          ( info.dom
          , spec->basics->vm.tag == hvm
          , info.guest_type == Guest64
          , 0 /* is_pae */
          , 0 /* is_nestedhvm */);

  if (err != 0) goto error_with_dom;

  switch (spec->basics->vm.tag) {

    case hvm:
      DB_DEBUG("[DB] Building an HVM guest\n");
      err = hvm_create_vm(&info, spec);
      break;

    case pv:
      err = setup_permissions(info.dom, &spec->basics->vm.u.pv);
      if (err != 0) goto error_with_dom;

      switch (info.guest_type) {
        case Guest64:
          DB_DEBUG("[DB] We have a 64-bit guest\n");
          err = create_vm_pv(x86_64)(hw_console, &info, spec, start_info_area);
          break;
        case Guest32pae:
          err = set_machine_address_size(info.dom, 36);
          if (err != 0) {
            DB_ERROR("[DB] Failed to set machine address size to 32 (%d)\n",
                                                                           err);
            goto error_with_dom;
          }

          DB_DEBUG("[DB] We have a 32-bit guest\n");
          err = set_address_size(info.dom, 32);
          if (err != 0) {
            DB_ERROR("[DB] Failed to set address size to 32 (%d)\n", err);
           goto error_with_dom;
          }
          err = create_vm_pv(x86_32pae)(hw_console, &info, spec,
                                                              start_info_area);
          break;

        default:
          DB_ERROR("[DB] Unknown guest type (%d)\n", info.guest_type);
          err = -EINVAL;
    }
  }

  if (err != 0) goto error_with_dom;

  // Fill in the result.
  spec->basics->domid  = info.dom;
  spec->guest_type    = info.guest_type;
  return 0;


error_with_dom:
  DB_DEBUG("Failed (%d), destroying domain #%d\n",err, info.dom);
  do {
    err1 = destroy_domain(info.dom);
  } while (err1 == -EAGAIN);

  if (err1 != 0) {
    DB_DEBUG("Failed to destroy domin (%d)\n", err1);
  }
  return err;
}



