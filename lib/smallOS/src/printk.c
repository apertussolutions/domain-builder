//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#include <stdio.h>
#include <string.h>
#include <smallOS/hypercall.h>
#include <smallOS/printk.h>
#include <smallOS/mem.h>
#include <smallOS/ring-buffer.h>

int printk(const char *fmt, ...)
{
    char buf[1024];
    va_list args;
    int err;
    va_start(args, fmt);
    (void)vsnprintf(buf, sizeof(buf), fmt, args);
    err = HYPERVISOR_console_io(CONSOLEIO_write, strlen(buf), buf);
    va_end(args);
    return err;
}

void init_virt_console(start_info_t *si, virt_console_t *con) {
  extern uint8_t _kernel_start[];
  con->data = (void*)_kernel_start + mfn_to_pfn(si->console.domU.mfn)
                                                              * PAGE_SIZE;
  con->port = si->console.domU.evtchn;
}

static inline
void write_chunk(virt_console_t *con, char* s, size_t len) {
  ring_buf_write( &con->data->out_cons, &con->data->out_prod
                , con->data->out, sizeof(con->data->out)
                , s, len
                , con->port
                );
}

int printc(virt_console_t *con, const char *fmt, ...) {
  char buf[1024];
  va_list args;
  char *from;

  va_start(args, fmt);
  (void)vsnprintf(buf, sizeof(buf), fmt, args);

  from = buf;
  do {
    char *to = strchr(from, '\n');
    if (to != NULL) {
      write_chunk(con, from, to - from);
      write_chunk(con, "\r\n", 2);
      from = to + 1;
    } else {
      write_chunk(con, from, strlen(from));
      break;
    }
  } while (1);

  va_end(args);
  return send_event(con->port);
}


