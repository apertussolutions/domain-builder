// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND

#include <errno.h>
#include <dom_builder.h>
#include <smallOS/domctl.h>
#include <smallOS/flask.h>
#include <string.h>
#include "db.h"


static
int enable_iomem(domid_t dom, const db_spec_range *range) {
  int err;
  mfn_t start_mfn = range->start;
  maddr_t start   = mfn_to_maddr(start_mfn);
  maddr_t end     = mfn_to_maddr(start_mfn + range->how_many - 1);
  end.num += 0xfffULL;
  unsigned allow  = range->enabled;

  DB_DEBUG("%s IO region 0x%llx--0x%llx\n",
                    allow? "Enable" : "Disable", start, end);
  err = iomem_perms(dom, start_mfn, range->how_many, allow);
  if (err != 0) {
    DB_ERROR("Failed to %s IO region 0x%llx--0x%llx (%d)\n",
                    allow? "enable" : "disable", start, end, err);
  }
  return err;
}


static
int enable_ioport(domid_t dom, const db_spec_range *range) {
  int err;
  unsigned start  = range->start;
  unsigned num    = range->how_many;
  unsigned end    = start + num - 1;
  unsigned allow  = range->enabled;

  DB_DEBUG("%s IO ports 0x%x--0x%x\n",
                    allow? "Enable" : "Disable", start, end);
  err = ioport_perms(dom, start, num, allow);
  if (err != 0) {
    DB_ERROR("Failed to %s IO ports 0x%x--0x%x (%d)\n",
                    allow? "enable" : "disable", start, end, err);
  }
  return err;
}


int setup_permissions(domid_t dom, const db_perms_t *spec) {
  int err,i,j;
  int irq = 0;

  if (!spec) {
    DB_ERROR("spec was NULL\n");
    return -EINVAL;
  }

  if (!spec->irq_bitmap && spec->irq_bitmap_size > 0) {
    DB_ERROR("irq_bitmap was NULL\n");
    return -EINVAL;
  }

  // Setup IRQ permissions
  DB_DEBUG("[DB] Setup IRQs: ");
  for(i = 0; i < spec->irq_bitmap_size; ++i) {
    for(j = 0; j < 8; ++j) {
      uint8_t mask = 1 << j;
      if (spec->irq_bitmap[i] & mask) {
        err = irq_perms(dom, irq, 1);
        if (err != 0) {
          DB_ERROR("Failed to enable IRQ %d (%d)\n", irq, err);
          return err;
        } else DB_DEBUG("%d ", irq);
      
      }
      irq++;
    }
  }
  DB_DEBUG("\n");

  // Setup IO memory permissions
  if (spec->iomem != NULL) {
    DB_DEBUG("[DB] Setting up IO memory\n");
    for (i = 0; i < spec->iomem_count; ++i) {
      err = enable_iomem(dom, &spec->iomem[i]);
      if (err != 0) return err;
    }
  }

  // Setup IO port permissions
  if (spec->ioport != NULL) {
    DB_DEBUG("[DB] Setting up IO ports\n");
    for (i = 0; i < spec->ioport_count; ++i) {
      err = enable_ioport(dom, &spec->ioport[i]);
      if (err != 0) return err;
    }
  }

  return 0;
}


