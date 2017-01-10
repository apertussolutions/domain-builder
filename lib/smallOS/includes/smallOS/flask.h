//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_FLASK_H__
#define __SMALLOS_FLASK_H__

#include <stdint.h>

// Extracted from flask.h
#define SECCLASS_XEN                                     1
#define SECCLASS_DOMAIN                                  2
#define SECCLASS_DOMAIN2                                 3
#define SECCLASS_HVM                                     4
#define SECCLASS_MMU                                     5
#define SECCLASS_RESOURCE                                6
#define SECCLASS_SHADOW                                  7
#define SECCLASS_EVENT                                   8
#define SECCLASS_GRANT                                   9
#define SECCLASS_SECURITY                                10
#define SECCLASS_IVC                                     11
#define SECCLASS_FILE                                    12
#define SECCLASS_DOMAINBUILDER                           13

#define FILE__READ                                0x00000001UL
#define FILE__WRITE                               0x00000002UL
#define FILE__CREATE                              0x00000004UL
#define FILE__EXECUTE                             0x00000008UL
#define FILE__RUNAS                               0x00000010UL

#define DOMAINBUILDER__BUILD                      0x00000001UL
#define DOMAINBUILDER__BUILDMEASURED              0x00000002UL

int flask_context_to_sid(uint32_t size, char *msg, uint32_t *sid);
int flask_sid_to_context(uint32_t *size, char *msg, uint32_t sid);

int flask_access(uint32_t ssid, uint32_t tsid,
                uint16_t tclass, uint32_t req,
                uint32_t *allowed, uint32_t *decided,
                uint32_t *auditallow, uint32_t *auditdeny,
                uint32_t *seqno);

int flask_getpeerid(evtchn_port_t port, uint32_t *sid);

int flask_create(uint32_t ssid, uint32_t tsid, uint32_t tclass, uint32_t *newsid);


int flask_access_simple(uint32_t ssid, uint32_t tsid, uint16_t tclass, uint32_t req);

#endif
