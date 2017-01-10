// BANNERSTART
// Copyright: 2011, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki (diatchki@galois.com)
// BANNEREND

#ifndef __SMALLOS_CHAN_H__
#define __SMALLOS_CHAN_H__

#include <smallOS/evtchn.h>
#include <smallOS/grant_table.h>

typedef struct {
  evtchn_port_t port;
  grant_ref_t ref;
} chan_msg_t;


typedef struct {
  volatile void *shared;  // page aligned
  evtchn_port_t port;
  int local;

  union {
    grant_handle_t handle; // used only on the receiving side
    grant_ref_t ref;
  };
} chan_t;


int connect_chan( domid_t dom             // in
                , chan_msg_t *msg         // in
                , void *shared            // in
                , chan_t *c               // out
                );


int new_chan( grant_entry_v2_t gtab[]        // in
            , size_t gtab_entries         // in
            , domid_t dom                 // in
            , void *shared                // in
            , chan_t *chan                // out
            , chan_msg_t *msg             // out
            );

int destroy_chan(grant_entry_v2_t tab[], chan_t c);


#endif


