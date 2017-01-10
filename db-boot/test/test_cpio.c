#include <stdio.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "cpio.h"

int main(void) {
  int f;
  struct stat s;
  int err;
  void *bytes;
  image img;

  f = open("test.cpio",O_RDONLY);
  if (f < 0) { perror("failed to open file"); return f; }

  err = fstat(f, &s);
  if (err < 0) { perror("failed to get file stats"); return err; }

  bytes = mmap(0, s.st_size, PROT_READ, MAP_SHARED, f, 0);
  if ((long)bytes < 0) { perror("failed to map file"); return (int)bytes; }

  printf("bytes0: %p\n", bytes);
  for(;;) {
    bytes = get_image(bytes,&img);
    if ((long)bytes > 0)
      printf("image: %p, %lu\n", bytes, img.size);
    else break;
  }

  return 0;
}


