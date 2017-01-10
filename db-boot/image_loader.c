#include "bzimage.h"
#include <zlib.h>
#include <string.h>
#include <smallOS/printk.h>

struct heap_status {
	size_t heapsize;
	void * heap;
};

static voidpf mymalloc(const voidpf opaque, const uInt items, const uInt size) {
	
	struct heap_status * const status = opaque;
	const size_t bytes = items * size;
	void *result = NULL;

        printk("mallocing %d bytes\n", bytes);
	
	if (bytes <= status->heapsize) {
		result = status->heap;
		status->heapsize -= bytes;
		status->heap += bytes;
	}
	return result;
}

static void myfree(voidpf opaque, voidpf address) {
}

static unsigned char allocspace[20*1024];
static struct heap_status heap = { .heapsize = sizeof(allocspace), .heap = allocspace};

int check_header(void * bzimage, uint64_t size) {
	struct setup_header *hdr = bzimage + bootloader_size + 1;

	char * magic_string = (char *) &hdr->header;

        if (size < bootloader_size + sizeof(struct setup_header) + 1)
          return 1;
	
	if (strncmp(HDR_MAGIC, magic_string, HDR_MAGIC_SZ)) {
		return 1;
	}
	
	if (hdr->version < 0x209) {
		//fprintf(stderr, "bzImage 2.09 or greater required\n");
		return 1;
	}
	
	if (hdr->loadflags != bzimage_loader_type) {
		//fprintf(stderr, "Not a bzImage\n");
		return 1;
	}
	
	if (hdr->jump <= 0x1ff) {
		//fprintf(stderr, "Not bzImage, jump too small\n");
		return 1;
	}
	return 0;
}

int test_elf(void *p, size_t len) {
	if (len < 5) {
		return 1;
	}
	
        unsigned char * c = p;
	// Check magic number
	if (c[0] != 127 || c[1] != 'E' || c[2] != 'L' || c[3] != 'F') {
     		printk("Missing ELF magic bytes %d %c %c %c\n", c[0], c[1], c[2], c[3]);
		return 1;
	}
	
/*
	// Test that this is 64-bit elf
	if ( ((uint8_t*)p)[4] != 2) {
		return 1;
	}
*/
	
	return 0;
}

int extract_bzimage(void *bzimage, size_t bzimage_size, void *out, size_t outlen) {
	
	struct setup_header *hdr = bzimage + bootloader_size + 1;
	int res;
	const int sector_size = 512;
	
	z_stream strm;
	strm.zalloc		= mymalloc;
	strm.zfree		= myfree;
	strm.opaque		= &heap;
	strm.avail_in	= hdr->payload_length;
	strm.next_in	= bzimage + (hdr->setup_sects+1) * sector_size + hdr->payload_offset;
	strm.avail_out	= outlen;
	strm.next_out	= out;
	
	if (check_header(bzimage, bzimage_size) != 0) {
		//fprintf(stderr, "bzImage's setup header incorrect\n");
		return -1;
	}
	
	if ((void*)strm.next_in + strm.avail_in > bzimage + bzimage_size) {
		//fprintf(stderr, "size mismatch\n");
		return -1;
	}
	
	res = inflateInit2(&strm, 16+MAX_WBITS);
    if (res != Z_OK) {
		//fprintf(stderr, "inflateInit2: %d\n", res);
		return -1;
	}
	
	res = inflate(&strm, Z_SYNC_FLUSH);
	if (res != Z_STREAM_END) {
		//fprintf(stderr, "Expected end of stream!\n");
		return -1;
	}
	
	res = inflateEnd(&strm);
	if (res != Z_OK) {
		//fprintf(stderr, "inflateEnd failed\n");
		return -1;
	}
	
	return strm.total_out;
}
