//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#include <string.h>
#include <stdio.h>
#include <smallOS/ring-buffer.h>
#include <smallOS/mem.h>
#include <smallOS/assert.h>

// Assumes that:
//  2^sizeof(size_t) % ring_buf_size == 0
inline
size_t ring_buf_read_non_block
  ( ring_buf_ix_t *cons_ptr, ring_buf_ix_t prod
  , void *ring_buf,          size_t ring_buf_size
  , void *out_buf,           size_t out_buf_size ) {

  size_t cons = *cons_ptr;
  size_t first, last;
  size_t used = 0;
  size_t todo;

  if (cons == prod) return 0;  // empty

  assert(ring_buf_size != 0);
  first = cons % ring_buf_size;
  last  = prod % ring_buf_size;


  if (last <= first) {
    todo = ring_buf_size - first;
    if (todo > out_buf_size) todo = out_buf_size;
    if (out_buf != NULL) { memcpy(out_buf, ring_buf + first, todo); mb(); }
    first = 0;
    used  = todo;

    *cons_ptr += todo;
  }

  todo = last - first;
  if (todo > out_buf_size - used) todo = out_buf_size - used;
  if (out_buf != NULL) { memcpy(out_buf + used, ring_buf + first, todo); mb(); }
  *cons_ptr += todo;
  used += todo;

  return used;
}

int ring_buf_read
  ( ring_buf_ix_t *cons_ptr, volatile ring_buf_ix_t *prod
  , void *ring_buf,          size_t ring_buf_size
  , void *out,               size_t out_size
  , evtchn_port_t data_port
  )
{
  size_t done = 0;

  while (done < out_size) {
    size_t this_time;
    this_time = ring_buf_read_non_block
                   ( cons_ptr, *prod
                   , ring_buf, ring_buf_size
                   , out == NULL ? out : out + done, out_size - done
                   );

    if (this_time == 0) {
      int err;

      err = send_event(data_port);
      if (err != 0) return err;

      err = receive_event(0, 1, &data_port);
      if (err != 0) return err;

    } else {
      done += this_time;
    }
  }

  return 0;
}



// Assumes that:
//  2^sizeof(size_t) % ring_buf_size == 0
inline
size_t ring_buf_write_non_block
  ( ring_buf_ix_t cons, ring_buf_ix_t *prod_ptr
  , void *ring_buf,     size_t ring_buf_size
  , void *in_buf,       size_t in_buf_size
  )
{
  size_t prod = *prod_ptr;
  size_t first, last;
  size_t used = 0;
  size_t todo;

  assert(ring_buf_size != 0);
  first = cons % ring_buf_size;
  last  = prod % ring_buf_size;
  if (first == last && cons != prod) return 0;  // full

  if (first <= last) {
    todo = ring_buf_size - last;
    if (todo > in_buf_size) todo = in_buf_size;

    memcpy(ring_buf + last, in_buf, todo);
    last  = 0;
    used  = todo;
    wmb();
    *prod_ptr += todo;
  }

  todo = first - last;
  if (todo > in_buf_size - used) todo = in_buf_size - used;
  memcpy(ring_buf + last, in_buf + used, todo);
  wmb();
  *prod_ptr += todo;
  used += todo;

  return used;
}


int ring_buf_write
  ( volatile ring_buf_ix_t *cons,  ring_buf_ix_t *prod
  , void *ring_buf,                size_t ring_buf_size
  , void *in_buf,                  size_t in_buf_size
  , evtchn_port_t data_port
  )
{
  size_t done = 0;

  while (done < in_buf_size) {
    size_t this_time;

    this_time = ring_buf_write_non_block ( *cons, prod
                                     , ring_buf, ring_buf_size
                                     , in_buf + done, in_buf_size - done
                                     );
    if (this_time == 0) {
      int err;

      err = send_event(data_port);
      if (err != 0) return err;

      err = receive_event(0, 1, &data_port);
      if (err != 0) return err;

    } else {
      done += this_time;
    }
  }

  return 0;
}




