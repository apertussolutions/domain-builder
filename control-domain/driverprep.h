#ifndef DRIVERPREP_H
#define DRIVERPREP_H

#include <stdlib.h>
#include <smallOS/xs.h>

int prepare_drivers(domid_t domid, void *cpio_file, size_t cpio_size, const char* filename, int*ishvm);

#endif
