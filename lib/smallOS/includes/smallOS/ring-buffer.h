//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef SMALLOS_RING_BUFFER_H
#define SMALLOS_RING_BUFFER_H

#include <stddef.h>
#include <smallOS/evtchn.h>

// We'd really like to be polymorphic in this...
// Forturnately, a bunch of the Xen services (console, xenstore)
// use a uint32_t type, so we also use this here.
typedef uint32_t ring_buf_ix_t;


size_t ring_buf_read_non_block
  ( ring_buf_ix_t *cons_ptr, ring_buf_ix_t prod
  , void *ring_buf,          size_t ring_buf_size
  , void *out_buf,           size_t out_buf_size );




// Read `out_buf_size` bytes from the ring-buffer.
// If there isn't enough data, then this function will:
//    1. Signal the port (to request data)
//    2. Block on the port (to wait until data is available).
//
// If the `out_buf` argument is NULL, then the data is read from the buffer
// but it is not stored anywhere.
//
// The function returns 0 on success, or an error code.
int ring_buf_read
  ( ring_buf_ix_t          *cons_ptr      // index where we start consuming
  , volatile ring_buf_ix_t *prod        // index up to where we can read (excl.)
  , void                   *ring_buf      // ring-buffer data area
  , size_t                 ring_buf_size  // size of ring-buffer data area
  , void                   *out_buf       // place data here (NULL -> ignore)
  , size_t                 out_buf_size   // how many bytes we need
  , evtchn_port_t          data_port      // use this to block when empty
  );



size_t ring_buf_write_non_block
  ( ring_buf_ix_t cons, ring_buf_ix_t *prod_ptr
  , void *ring_buf,     size_t ring_buf_size
  , void *in_buf,       size_t in_buf_size
  );


// Write `in_buf_size` bytes to the ring-buffer.
// If there isn't enough space, then this function will:
//    1. Signal the port (to let them know they should read a bit)
//    2. Block on the port (to wait until space is available).
// port, and wait for some space to be freed.
// The function returns 0 on success, or an error code.
int ring_buf_write
  ( volatile ring_buf_ix_t *cons      // index up to where we can write (excl.)
  , ring_buf_ix_t          *prod      // index where we start writing
  , void                   *ring_buf      // ring-buffer data area
  , size_t                 ring_buf_size  // size of ring-buffer data area
  , void                   *in_buf        // get data from here
  , size_t                 in_buf_size    // how much data we need to write
  , evtchn_port_t          data_port      // use this to block when full
  );

#endif

