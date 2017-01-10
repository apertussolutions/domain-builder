//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_PRINTK_H__
#define __SMALLOS_PRINTK_H__

#include <smallOS/xen-version.h>
#include <smallOS/evtchn.h>
#include <xen/io/console.h>


int printk(const char *fmt, ...);

typedef struct {
  struct xencons_interface *data;
  evtchn_port_t port;
} virt_console_t;


void init_virt_console(start_info_t *si, virt_console_t *con);
int printc(virt_console_t *con, const char *fmt, ...);

#endif

