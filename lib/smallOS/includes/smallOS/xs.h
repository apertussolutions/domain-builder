#ifndef __SMALLOS_XS_H__
#define __SMALLOS_XS_H__

#include <stdint.h>
#include <xen/io/xs_wire.h>
#include <xen/xen.h>

typedef struct {
  struct xenstore_domain_interface *ring;
  evtchn_port_t port;
} xs_chan_t;

int xs_introduce( xs_chan_t *xs
                , domid_t domid, unsigned long mfn, unsigned long port);

int xs_start_transaction( xs_chan_t *xs, uint32_t *txid );

// End transaction txid:
// commit == 0: rollback transaction
// otherwise  : commit transaction
int xs_end_transaction( xs_chan_t *xs, uint32_t txid, int commit );

#define NO_TRANSACTION 0

int xs_command
  ( xs_chan_t *xs
  , enum xsd_sockmsg_type msg_type
  , uint32_t txid
  , void *rsp, size_t *rsp_size
  , ...
  ) __attribute__ ((sentinel));

#endif
