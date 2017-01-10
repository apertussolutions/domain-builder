// BANNERSTART
// Copyright: 2011, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki (diatchki@galois.com)
// BANNEREND

#ifndef __CPIO_H__
#define __CPIO_H__

typedef struct {
  unsigned long size;
  char*         name;
  void*         bytes;
} image;


// Try to get n image out of a CPIO file.
// Returns:
//  ptr to next entry, if successful
//  (void*)0,          if there were no more images
//  (void*)(-1),       if we found a malformed entry
const void* get_image(const void *src, unsigned long size, image *out);

// Find a specific image in a CPIO.
// Searches directly in the CPIO, without the need to make an index.
// Returns:
//    0,          if successfule
//    -ENOENT     if we did not find the file
//    -EINVAL     if either the arguments were invalid,
//                   or the CPIO was malformed
int find_file_cpio( const void *src, unsigned long size
                  , const char* name
                  , image *out
                  );


// Build an index out of a CPIO file.
int index_cpio( const void *src, unsigned long size
              , image out[], unsigned long max_entries
              );

// Find an image in a CPIO index.
image *find_file(const char *name, image out[], unsigned long n);



#endif


