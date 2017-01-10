#ifndef CMDLINES_H
#define CMDLINES_H

#include <stdlib.h>
#include <stdint.h>
#include <xen/xen.h>
#include <smallOS/evtchn.h>

#define SCHEMA_FILENAME_SIZE 40
#define MAX_INTROS           16

typedef struct {
  domid_t       domid;
  mfn_t         mfn;
  evtchn_port_t port;
} intro_t;

typedef struct {
  int svp_mode;                                 // Are we booting SVP or UVP?
  char schema_filename[SCHEMA_FILENAME_SIZE+1]; // SVP/UVP

  domid_t db_dom;             // SVP/UVP
  domid_t host_store_domain;  // SVP/UVP
  domid_t drivers_dom;        // SVP/UVP
  domid_t xenstore_domain;    // SVP
  domid_t ctrl_dom;           // SVP/UVP
  domid_t vtpm_manager_dom;   // SVP

  char * db_dm;           // SVP/UVP
  char * host_store_dm;   // SVP/UVP
  char * drivers_dm;      // SVP/UVP
  char * xenstore_dm;     // SVP
  char * ctrl_dm;         // SVP/UVP
  char * vtpm_manager_dm; // SVP

  unsigned long intro_num;    // SVP: Xenstore introductions.
  intro_t intros[MAX_INTROS]; // Usually, there are only 4 of these.
} cmdline_opts_t;

// Manipulating guest's command lines.
int substitute_cmdline(char * str, domid_t ctrl_dom);

int load_cmdline(char * args, cmdline_opts_t *opts);
#endif
