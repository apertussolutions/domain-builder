//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_DOMCTL_H__
#define __SMALLOS_DOMCTL_H__

#include <smallOS/xen-version.h>
#include <xen/domctl.h>
#include <smallOS/hypercall.h>
#include <string.h>       // for memset

// Unpause a domain.
static inline
int unpause_domain (domid_t domid) {
  xen_domctl_t command;
  memset(&command, 0, sizeof(command));
  command.cmd                 = XEN_DOMCTL_unpausedomain;
  command.interface_version   = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain              = domid;
  return HYPERVISOR_domctl(&command);
}


// Destroy an existing domain.
static inline
int destroy_domain (domid_t domid) {
  xen_domctl_t command;
  memset(&command, 0, sizeof(command));
  command.cmd               = XEN_DOMCTL_destroydomain;
  command.interface_version = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain            = domid;
  return HYPERVISOR_domctl(&command);
}

static inline int set_virq_handler(domid_t domid, int virq)
{
  xen_domctl_t command;
  memset(&command, 0, sizeof(command));
  command.cmd               = XEN_DOMCTL_set_virq_handler;
  command.interface_version = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain            = domid;
  command.u.set_virq_handler.virq = virq;
  return HYPERVISOR_domctl(&command);
}

#endif
