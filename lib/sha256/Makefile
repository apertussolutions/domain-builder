# BANNERSTART
# Copyright: 2011, Galois, Inc.
# License-file: LICENSE
# Author: Iavor S. Diatchki (diatchki@galois.com)
# BANNEREND

TOP=../..
include $(TOP)/mk/config

all: libsha256.a

INCLUDE_DIRS=includes ../c/includes

include $(TOP)/mk/compile

CFLAGS+=-O3 -DNDEBUG -fomit-frame-pointer


sha256.o: sha256.c includes/sha256.h includes/BV.h block.c
	$(call cmd,CC) $(CPPFLAGS) $(CFLAGS) -c sha256.c -o $@

libsha256.a: sha256.o
	$(call cmd,AR) rcs $@ $(filter %.o, $^)


testDriver: testDriver.c includes/sha256.h libsha256.a
	$(call cmd,CC) $(CPPFLAGS) $(CFLAGS) testDriver.c -L. -lsha256 -o $@


.PHONY : all clean

clean:
	rm -f *.o testDriver libsha256.a


