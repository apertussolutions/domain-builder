// BANNERSTART
// - Copyright 2008, Galoic, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND


#ifndef DOM_BUILDER_H
#define DOM_BUILDER_H

#include <smallOS/xen-version.h>
#include <smallOS/evtchn.h>
#include <smallOS/mem.h>
#include <xen/xen.h>
#include <db-api/types.h>


typedef enum { Guest64, Guest32pae } db_guest_t;

typedef struct {
  uint64_t  domid;    // IN:  domid of service, or ~0ULL for none.
  uint64_t  port;     // OUT
  uint64_t  mfn;      // OUT
} db_service_t;


typedef struct {
  db_spec *basics;                 // see $(TOP)/protocols/db-spec.h

  db_service_t console;
  db_service_t xenstore;

  db_guest_t guest_type;            // OUT: What sort of guest we built.

  size_t  image_size;
  void    *image;

  size_t  ram_disk_size;
  void    *ram_disk;

} db_build_spec;


// Setup the initial state for a domain.
int create_vm                     // returns status (error code)
  ( void *start_info              // Where to map start_info, for further init.
  , db_build_spec *spec           // IN/OUT: Specs for basic build.
                                  // If successful, some of the fields
                                  // are filled in.
  , dom0_vga_console_info_t *hw_console
  , uint32_t sid
  );


#endif
