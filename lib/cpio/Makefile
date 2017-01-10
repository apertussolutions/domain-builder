# BANNERSTART
# Copyright: 2011, Galois, Inc.
# License-file: LICENSE
# Author: Iavor S. Diatchki (diatchki@galois.com)
# BANNEREND

TOP=../..
include $(TOP)/mk/config

TGT           =libcpio.a
all: $(TGT)

LIBC          =../c
INCLUDE_DIRS  =includes $(LIBC)/includes

include $(TOP)/mk/compile

OBJS          =build/cpio.o

$(TGT): $(OBJS)
	$(call cmd,AR) rcs $@ $(filter %.o, $^)

build/%.o: src/%.c $(INCLUDE_FILES)
	$Q $(MKDIR) -p $(dir $@)
	$(call cmd,CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean:
	$(RM) -rf $(TGT) build


.PHONY: all clean
