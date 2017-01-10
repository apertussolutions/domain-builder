# BANNERSTART
# Copyright: 2011, Galois, Inc.
# License-file: LICENSE
# Author: Iavor S. Diatchki (diatchki@galois.com)
# BANNEREND

TOP=../..
include $(TOP)/mk/config

TGTS          =start.o libsmallOS.a
all: $(TGTS)

LIBC          =../c

INCLUDE_DIRS  =includes $(LIBC)/includes $(XEN_HEADERS)

include $(TOP)/mk/compile

OBJS          =callback.o events.o evtchn.o printk.o grant_table.o assert.o \
               shared_info.o time.o flask.o mem.o ring-buffer.o reset_virt.o \
               xs.o

libsmallOS.a: $(addprefix build/,$(OBJS))
	$(call cmd,AR) rcs $@ $(filter %.o, $^)

build/%.o: src/%.c $(INCLUDE_FILES)
	$Q $(MKDIR) -p $(dir $@)
	$(call cmd,CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

build/%.o: src/%.S $(INCLUDE_FILES)
	$Q $(MKDIR) -p $(dir $@)
	$(call cmd,CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

start.o: build/start.o
	$Q $(CP) $< $@

clean:
	$(RM) -rf $(TGTS) build

.PHONY: all clean
