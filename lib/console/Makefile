# BANNERSTART
# Copyright: 2011, Galois, Inc.
# License-file: LICENSE
# Author: Iavor S. Diatchki (diatchki@galois.com)
# BANNEREND

TOP=../..
include $(TOP)/mk/config

TGT=libconsole.a
all: $(TGT)

LIBC        =../c
LIBSMALLOS  =../smallOS

INCLUDE_DIRS=includes $(LIBSMALLOS)/includes $(LIBC)/includes $(XEN_HEADERS)

include $(TOP)/mk/compile

OBJS=$(patsubst src/%.c,build/%.o,$(wildcard src/*.c))
$(TGT): $(OBJS)
	$(call cmd,AR) rcs $@ $(filter %.o, $^)

build/%.o: src/%.c $(INCLUDE_FILES)
	$Q $(MKDIR) -p build
	$(call cmd,CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

.PHONY: all clean
clean:
	-$(RM) -rf $(TGT) build

