//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#include <smallOS/hypercall.h>
#include <xen/xsm/flask_op.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>


int flask_context_to_sid(uint32_t size, char *msg, uint32_t *sid) {
  int err;
  xen_flask_op_t op;
  if (size < sizeof(uint32_t)) return -775;

  op.interface_version     = XEN_FLASK_INTERFACE_VERSION;
  op.cmd                   = FLASK_CONTEXT_TO_SID;
  op.u.sid_context.size    = size;
  set_xen_guest_handle(op.u.sid_context.context,msg);

  err = HYPERVISOR_xsm_op(&op);
  if (err != 0) return err;

  *sid = op.u.sid_context.sid;
  return 0;
}

int flask_sid_to_context(uint32_t *size, char *msg, uint32_t sid) {
    int err;
    xen_flask_op_t op
      = { .interface_version     = XEN_FLASK_INTERFACE_VERSION
        , .cmd                   = FLASK_SID_TO_CONTEXT
        , .u.sid_context.size    = *size
        , .u.sid_context.sid     = sid
        };
    set_xen_guest_handle(op.u.sid_context.context,msg);

    err = HYPERVISOR_xsm_op(&op);
    if (err == 0) {
      *size = op.u.sid_context.size;
    }

    return err;
}

int flask_access(uint32_t scon, uint32_t tcon,
                uint16_t tclass, uint32_t req,
                uint32_t *allowed, uint32_t *decided,
                uint32_t *auditallow, uint32_t *auditdeny,
                uint32_t *seqno)
{

    int err;
    xen_flask_op_t op
      = { .interface_version     = XEN_FLASK_INTERFACE_VERSION
        , .cmd                   = FLASK_ACCESS
        , .u.access.ssid           = scon
        , .u.access.tsid           = tcon
        , .u.access.tclass         = tclass
        , .u.access.req            = req
        };

    err = HYPERVISOR_xsm_op(&op);
    if (err != 0) {
      return err;
    }

    *allowed     = op.u.access.allowed;
    *auditallow = op.u.access.audit_allow;
    *auditdeny  = op.u.access.audit_deny;
    *seqno       = op.u.access.seqno;
    
    err = ((op.u.access.allowed & req) == req)? 0 : -EPERM;
    return err;
}

int flask_getpeerid(evtchn_port_t port, uint32_t *sid) {
    xen_flask_op_t op
      = { .interface_version     = XEN_FLASK_INTERFACE_VERSION
        , .cmd                   = FLASK_GET_PEER_SID
        , .u.peersid.evtchn      = port
        };

    int err = HYPERVISOR_xsm_op(&op);
    if (err == 0) {
      *sid = op.u.peersid.sid;
    }

    return err;
}

int flask_create(uint32_t ssid, uint32_t tsid, uint32_t tclass, uint32_t *newsid)
{
    xen_flask_op_t op
      = { .interface_version     = XEN_FLASK_INTERFACE_VERSION
        , .cmd                   = FLASK_CREATE
        , .u.transition.ssid     = ssid
        , .u.transition.tsid     = tsid
        , .u.transition.tclass   = tclass
        };

    int err = HYPERVISOR_xsm_op(&op);
    if (err == 0) {
      *newsid = op.u.transition.newsid;
    }

    return err;
}


int flask_access_simple(uint32_t ssid, uint32_t tsid, uint16_t tclass, uint32_t req)
{
    uint32_t allowed, decided, auditallow, auditdeny, seqno;
    return flask_access(ssid, tsid, tclass, req, &allowed, &decided, &auditallow, &auditdeny, &seqno);
}
