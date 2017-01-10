#ifndef CONTORL_DOMAIN
#define CONTORL_DOMAIN

#include <stdint.h>
#include <xen/xen.h>

typedef struct {
  domid_t domid;
  char    *name;
} platform_member_info;

#endif
