// BANNERSTART
// Copyright: 2011, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki (diatchki@galois.com)
// BANNEREND

#include <smallOS/chan.h>
#include <smallOS/grant_table.h>
#include <smallOS/evtchn.h>

int new_chan( grant_entry_v2_t gtab[]     // in
            , size_t gtab_entries         // in
            , domid_t dom                 // in
            , void *shared                // in
            , chan_t *chan                // out
            , chan_msg_t *msg             // out
            ) {

  int err;
  grant_ref_t slot;
  evtchn_port_t port;

  if (grant_table_find_slots(gtab, gtab_entries, &slot, 1) != 1) return -1;

  err = bind_unbound(dom,&port);
  if (err != 0) return err;

  grant_table_set(gtab, slot, dom, shared, GTF_permit_access);

  chan->port   = port;
  chan->shared = shared;
  chan->local  = 1;
  chan->ref    = slot;

  msg->port = port;
  msg->ref = slot;

  return 0;
}


int connect_chan(domid_t dom, chan_msg_t *msg, void *shared, chan_t *c) {
  int err;
  grant_handle_t h;
  evtchn_port_t port;

  err = map_grant_ref(dom, msg->ref, shared, &h);
  if (err < 0) return err;

  err = bind_interdomain(dom, msg->port, &port);
  if (err < 0) {
    unmap_grant_handles(&h, shared, 1);
    return err;
  }

  c->port   = port;
  c->shared = shared;
  c->local  = 0;
  c->handle = h;

  return 0;
}

int destroy_chan(grant_entry_v2_t tab[], chan_t c) {
  int err1, err2;

  err1 = unbind(c.port);

  if (c.local) {
    grant_table_free(tab, c.ref);
    err2 = 0;
  } else {
    err2 = unmap_grant_handles(&c.handle, (void*)&c.shared, 1);
  }

  return err1 == 0 ? err2 : err1;
}
