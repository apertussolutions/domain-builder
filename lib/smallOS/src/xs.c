#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <smallOS/ring-buffer.h>
#include <smallOS/mem.h>
#include <smallOS/evtchn.h>
#include <smallOS/xs.h>

inline
int xs_introduce( xs_chan_t *xs
                , domid_t domid, unsigned long mfn, unsigned long port) {

  return xs_command(xs,
    XS_INTRODUCE, 0,
    NULL, NULL,
    "%lu", (unsigned long) domid,
    "%lu", mfn,
    "%lu", port,
    NULL);
}

int xs_start_transaction( xs_chan_t *xs, uint32_t *txid ) {
  char result[20] = {0};
  size_t len = sizeof(result);
  int ret = xs_command(xs,
              XS_TRANSACTION_START, NO_TRANSACTION,
              result, &len,
              "",
              NULL);
  if (ret >= 0) *txid = atoi(result);
  return ret;
}

int xs_end_transaction( xs_chan_t *xs, uint32_t txid, int commit ) {
  int err;

  char buf[10] = {0};
  size_t len = sizeof(buf);

  err = xs_command(xs,
              XS_TRANSACTION_END, txid,
              buf, &len,
              commit ? "T" : "F",
              NULL);

  if (err == -EREMOTEIO && strcmp("EAGAIN", buf) == 0) {
    return -EAGAIN;
  }
  return err;
}

static inline
int xs_write(xs_chan_t *xs, void *buf, size_t size) {
  return ring_buf_write ( &xs->ring->req_cons, &xs->ring->req_prod
                        , xs->ring->req, sizeof(xs->ring->req)
                        , buf, size
                        , xs->port
                        );
}

static inline
int xs_read(xs_chan_t *xs, void *buf, size_t size) {
  return ring_buf_read( &xs->ring->rsp_cons, &xs->ring->rsp_prod
                      , xs->ring->rsp, sizeof(xs->ring->rsp)
                      , buf, size
                      , xs->port
                      );
}


static inline
int do_xs_command( xs_chan_t *xs
  , enum xsd_sockmsg_type msg_type
  , uint32_t txid
  , size_t param_size, void *param
  , void *rsp, size_t *rsp_size
  ) {

  const uint32_t req_id = 78;   // arbitrary
  struct xsd_sockmsg msg_header
    = { .type = msg_type
      , .req_id = req_id
      , .tx_id = txid
      , .len = param_size
      };

  size_t rcv_todo;
  int err;

  err = xs_write(xs, &msg_header, sizeof(msg_header));
  if (err != 0) return err;

  err = xs_write(xs, param, param_size);
  if (err != 0) return err;

  err = send_event(xs->port);
  if (err != 0) return err;

  memset(&msg_header, 0, sizeof(msg_header));

  err = xs_read(xs, &msg_header, sizeof(msg_header));
  if (err != 0) return err;
  if (msg_header.req_id != req_id) return -ENXIO;   // or something...

  rcv_todo = msg_header.len;

  if (rsp_size != NULL) {
    if (rcv_todo > *rsp_size) rcv_todo = *rsp_size;

    err = xs_read(xs, rsp, rcv_todo);
    if (err != 0) return err;
  } else {
    rcv_todo = 0;
  }

  // Do we need to truncate stuff?
  rcv_todo = msg_header.len - rcv_todo;
  if (rcv_todo > 0) {
    err = xs_read(xs, NULL, rcv_todo);
    if (err != 0) return err;
  }

  // Return:
  //  0:              all good
  //  -EREMOTEIO:     XS reported an error.
  //                  (possibly truncated details in the payload)
  //  -E2BIG:         no remote error, but we had to truncate the mssage.
  //
  // XXX: Maybe we could provide some sort of "read more" API instead
  // of truncating the payload?
  if (rsp_size != NULL) *rsp_size = msg_header.len;
  return msg_header.type == XS_ERROR ? -EREMOTEIO :
         rcv_todo > 0                ? -E2BIG     : 0;
}


int xs_command( xs_chan_t *xs
              , enum xsd_sockmsg_type msg_type
              , uint32_t txid
              , void *rsp, size_t *rsp_size
              , ...
              ) {
  va_list ap;
  char msg[XENSTORE_PAYLOAD_MAX];
  const char * fmt;
  size_t used = 0;

  va_start(ap, rsp_size);

  while ( (fmt = va_arg(ap, const char *)) != NULL) {
    size_t have = XENSTORE_PAYLOAD_MAX - used;
    int err = vsnprintf(&msg[used], have, fmt, ap);
    if (err < 0) return err;
    if (err >= have) return -E2BIG;   // We ran out of space.
    used += err + 1;    // 1 for the \0 at the end.
  }

  va_end(ap);

  if (msg_type == XS_WRITE) used--;  // don't count the \0 terminator.
  return do_xs_command(xs, msg_type, txid, used, msg, rsp, rsp_size);
}



