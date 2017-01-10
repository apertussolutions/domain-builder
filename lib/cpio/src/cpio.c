// BANNERSTART
// Copyright: 2011, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki (diatchki@galois.com)
// BANNEREND

#include "cpio.h"
#include <string.h>
#include <errno.h>

typedef struct {
  char magic   [6];
  char dev     [6];
  char ino     [6];
  char mode    [6];
  char uid     [6];
  char gid     [6];
  char nlink   [6];
  char rdev    [6];
  char mtime   [11];
  char namesize[6];
  char filesize[11];
} cpio_odc_header;

static inline
int decode(const char* data, unsigned long size, unsigned *out) {
  unsigned long i;
  unsigned num = 0;

  for (i = 0; i < size; ++i) {
    unsigned digit;

    if (data[i] < '0' || data[i] >= '8') return -EINVAL;

    digit = (unsigned)(data[i] - '0');
    num = (num * 8) + digit;
  }

  *out = num;
  return 0;
}

const void* get_image(const void *src, unsigned long size, image *out) {
  const cpio_odc_header *h;
  unsigned namesz;
  unsigned filesz;
  const char *name;
  const void *data;

  if (size < sizeof(cpio_odc_header)) goto ERR;

  h         = src;
  name      = src + sizeof(cpio_odc_header);

  if (decode(h->namesize,6,&namesz) != 0) goto ERR;
  if (decode(h->filesize,11,&filesz) != 0) goto ERR;

  if (size < sizeof(cpio_odc_header) + namesz + filesz) goto ERR;

  data      = name + namesz;

  if (strncmp(h->magic, "070707",6) != 0) goto ERR;
  if (filesz == 0 && namesz == 11 &&
      strncmp(name,"TRAILER!!!",11) == 0) return (void*)0;

  out->size  = filesz;
  out->name  = (char*)name;
  out->bytes = (void*)data;

  return data + filesz;

ERR:
  return (void*) (-1);
}

int index_cpio( const void *src, unsigned long size
              , image out[], unsigned long max_entries) {

  unsigned long entries = 0;
  if (src == NULL) return entries;

  while (entries < max_entries) {
    const void *new_src = get_image(src, size, &out[entries]);
    if (new_src == (void*)-1) return -EINVAL;
    if (new_src == NULL)      return entries;
    ++entries;
    size -= new_src - src;
    src   = new_src;
  }
  return entries;
}

image *find_file(const char *name, image out[], unsigned long n) {
  unsigned long i;
  for (i = 0; i < n; i++) {
    if (!strcmp(name, out[i].name)) {
      return &out[i];
    }
  }
  return (void*)0;
}


int find_file_cpio( const void *cpio, unsigned long size
                  , const char *name, image *out) {

  if (name == NULL || out == NULL) return -EINVAL;
  if (cpio == NULL) return -ENOENT;

  for(;;) {
    const void *new_cpio = get_image(cpio, size, out);
    if (new_cpio == (void*)-1) return -EINVAL;
    if (new_cpio == NULL)      return -ENOENT;
    if (strcmp(out->name,name) == 0) return 0;
    size -= new_cpio - cpio;
    cpio  = new_cpio;
  }
}



