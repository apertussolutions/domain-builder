// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND


/*
 * This module contains functions that perform hyper-calls on the
 * behalf of the domain builder (e.g., create a new virtual machine,
 * allocate memory, mark page tables, etc.)
 *
 * At the moment, this module uses code from mini-os to perform the
 * hyper-calls (see hypervisor.h).
 *
 */


#include <string.h>         // for memset
#include <smallOS/hypercall.h>
#include <smallOS/domctl.h>

#include <xen/memory.h>     // for memory structs
#include <xen/hvm/hvm_op.h> // for hvm    structs
#include "db.h"

int set_cpuid
  ( domid_t domid
  , const unsigned int *input
  , const unsigned int *regs
  ) {
    xen_domctl_t domctl;
    memset(&domctl, 0, sizeof (domctl));
    domctl.domain = domid;
    domctl.interface_version = XEN_DOMCTL_INTERFACE_VERSION;
    domctl.cmd = XEN_DOMCTL_set_cpuid;
    domctl.u.cpuid.input[0] = input[0];
    domctl.u.cpuid.input[1] = input[1];
    domctl.u.cpuid.eax = regs[0];
    domctl.u.cpuid.ebx = regs[1];
    domctl.u.cpuid.ecx = regs[2];
    domctl.u.cpuid.edx = regs[3];

    return HYPERVISOR_domctl(&domctl);
}


int set_hvm_param(domid_t dom, uint32_t param, uint64_t val) {
  xen_hvm_param_t cmd;
  memset(&cmd, 0, sizeof(cmd));
  cmd.domid = dom;
  cmd.index = param;
  cmd.value = val;
  return HYPERVISOR_hvm_op(HVMOP_set_param, &cmd);
}

int set_machine_address_size(domid_t dom, uint32_t size) {
  xen_domctl_t command;
  memset(&command, 0, sizeof(command));

  command.cmd                 = XEN_DOMCTL_set_machine_address_size;
  command.interface_version   = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain              = dom;
  command.u.address_size.size = size;

  return HYPERVISOR_domctl(&command);
}



int set_address_size(domid_t dom, uint32_t size) {
  xen_domctl_t command;
  memset(&command, 0, sizeof(command));

  command.cmd                 = XEN_DOMCTL_set_address_size;
  command.interface_version   = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain              = dom;
  command.u.address_size.size = size;

  return HYPERVISOR_domctl(&command);
}


// Request a number of MFNs for the use of the domain.
int populate_physmap(domid_t domid, unsigned int num_pages, xen_pfn_t *ptr)
{
  xen_memory_reservation_t reservation;

  set_xen_guest_handle(reservation.extent_start, ptr);
  reservation.nr_extents   = num_pages;
  reservation.extent_order = 0;
#if __XEN_INTERFACE_VERSION__ >= 0x00030209
  reservation.mem_flags = 0;
#else
  reservation.address_bits = 0;
#endif
  reservation.domid        = domid;

  return HYPERVISOR_memory_op(XENMEM_populate_physmap, &reservation);
}

// Allocate some pages
int allocate_superpages
  ( domid_t domid
  , unsigned int num_pages
  , unsigned int extent_order
  , xen_pfn_t *ptr
  )
{
  xen_memory_reservation_t reservation;

  set_xen_guest_handle(reservation.extent_start, ptr);
  reservation.nr_extents   = num_pages;
  reservation.extent_order = extent_order;
#if __XEN_INTERFACE_VERSION__ >= 0x00030209
  reservation.mem_flags = 0;
#else
  reservation.address_bits = 0;
#endif
  reservation.domid        = domid;

  return HYPERVISOR_memory_op(XENMEM_populate_physmap, &reservation);
}


int mark_as_page_table(domid_t dom, pt_level_t level, xen_pfn_t mfn) {
  mmuext_op_t pin_request;

  switch(level) {
    case L1: pin_request.cmd = MMUEXT_PIN_L1_TABLE; break;
    case L2: pin_request.cmd = MMUEXT_PIN_L2_TABLE; break;
    case L3: pin_request.cmd = MMUEXT_PIN_L3_TABLE; break;
    case L4: pin_request.cmd = MMUEXT_PIN_L4_TABLE; break;
    default:
      return -1;
  }

  pin_request.arg1.mfn = mfn;
  return HYPERVISOR_mmuext_op(&pin_request, 1, NULL, dom);
}


// Create a new domain, with an automatically assigned domain id.
int create_domain
  ( domid_t domid
  , uint32_t ssidref
  , uint32_t flags
  , domid_t *dom
  ) {
  xen_domctl_t command;
  int err;

  memset(&command, 0, sizeof(command));
  command.cmd               = XEN_DOMCTL_createdomain;
  command.interface_version = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain            = domid;
  command.u.createdomain.ssidref = ssidref;
  command.u.createdomain.flags = flags;

  err = HYPERVISOR_domctl(&command);
  if (err == 0) *dom = command.domain;
  return err;
}


// Get the shared info frame for a given domain.
// Returns a negative error, or 0 on success.
// The result is passed in the argument "res".
int get_shared_frame(domid_t domid, mfn_t* res) {
  xen_domctl_t command;
  int err;
  memset(&command, 0, sizeof(command));
  command.cmd               = XEN_DOMCTL_getdomaininfo;
  command.interface_version = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain            = domid;
  err = HYPERVISOR_domctl(&command);
  if (err < 0) return err;
  *res = command.u.getdomaininfo.shared_info_frame;
  return 0;
}


// Set the max. number of CPUs usable by the domain.
int set_max_vcpus (domid_t domid, uint32_t max) {
  xen_domctl_t command;
  memset(&command, 0, sizeof(command));
  command.cmd               = XEN_DOMCTL_max_vcpus;
  command.interface_version = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain            = domid;
  command.u.max_vcpus.max   = max;
  return HYPERVISOR_domctl(&command);
}


// Set the max. amount of memory (in KB) usable by the domain.
int set_max_mem (domid_t domid, uint64_t max) {
  xen_domctl_t command;
  memset(&command, 0, sizeof(command));
  command.cmd               = XEN_DOMCTL_max_mem;
  command.interface_version = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain            = domid;
  command.u.max_mem.max_memkb = max;
  return HYPERVISOR_domctl(&command);
}


// Set the context for a VCPU.
// The second argument is "void*" because we use it with different
// types, depending on the type of guest that we are building.
int set_vcpu_context(domid_t domid, uint32_t vcpu, void* ctxt) {
  xen_domctl_t command;
  memset(&command, 0, sizeof(command));
  command.cmd                 = XEN_DOMCTL_setvcpucontext;
  command.interface_version   = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain              = domid;
  command.u.vcpucontext.vcpu  = vcpu;
  set_xen_guest_handle(command.u.vcpucontext.ctxt, ctxt);
  return HYPERVISOR_domctl(&command);
}


// Set IO memory permissions.
int iomem_perms(domid_t domid, mfn_t mfn, unsigned long num, unsigned allow) {
  xen_domctl_t command;
  memset(&command, 0, sizeof(command));
  command.cmd                 = XEN_DOMCTL_iomem_permission;
  command.interface_version   = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain              = domid;
  command.u.iomem_permission.first_mfn    = mfn;
  command.u.iomem_permission.nr_mfns      = num;
  command.u.iomem_permission.allow_access = allow;
  return HYPERVISOR_domctl(&command);
}

// Set IO port permissions.
int ioport_perms(domid_t domid, unsigned port, unsigned num, unsigned allow) {
  xen_domctl_t command;
  memset(&command, 0, sizeof(command));
  command.cmd                 = XEN_DOMCTL_ioport_permission;
  command.interface_version   = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain              = domid;
  command.u.ioport_permission.first_port    = port;
  command.u.ioport_permission.nr_ports      = num;
  command.u.ioport_permission.allow_access  = allow;
  return HYPERVISOR_domctl(&command);
}

// Set IRQ permissions.
int irq_perms(domid_t domid, unsigned irq, unsigned allow) {
  xen_domctl_t command;
  memset(&command, 0, sizeof(command));
  command.cmd                 = XEN_DOMCTL_irq_permission;
  command.interface_version   = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain              = domid;
  command.u.irq_permission.pirq         = irq;
  command.u.irq_permission.allow_access = allow;
  return HYPERVISOR_domctl(&command);
}



int hypercall_init (domid_t domid, mfn_t mfn) {
  xen_domctl_t command;
  memset(&command, 0, sizeof(command));
  command.cmd               = XEN_DOMCTL_hypercall_init;
  command.interface_version = XEN_DOMCTL_INTERFACE_VERSION;
  command.domain            = domid;
  command.u.hypercall_init.gmfn = mfn;
  return HYPERVISOR_domctl(&command);
}


int set_shadow_allocation(domid_t domid, uint32_t mbs) {
    xen_domctl_t domctl;

    memset(&domctl,0,sizeof(domctl));

    domctl.cmd               = XEN_DOMCTL_shadow_op;
    domctl.interface_version = XEN_DOMCTL_INTERFACE_VERSION;
    domctl.domain            = domid;
    domctl.u.shadow_op.op    = XEN_DOMCTL_SHADOW_OP_SET_ALLOCATION;
    domctl.u.shadow_op.mb    = mbs;

    return HYPERVISOR_domctl(&domctl);
}
