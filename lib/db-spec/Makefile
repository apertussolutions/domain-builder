# BANNERSTART
# Copyright: 2011 Galois Inc.
# License-file: LICENSE
# Author: Iavor S. Diatchki (diatchki@galois.com)
# BANNEREND

TOP=../..
include $(TOP)/mk/config

LIBC=../c
LIBSHA=../sha256

INCLUDE_DIRS  =includes $(LIBC)/includes $(XEN_HEADERS)
TGT           =libdb-spec.a
all: $(TGT)

include $(TOP)/mk/compile

OBJS=$(patsubst src/%.c,build/%.o, $(wildcard src/*.c)) \
     $(foreach i,encode decode,build/db-api_$i.o)

.PHONY: all

all: $(TGT)

$(TGT): $(OBJS)
	$(call cmd,AR) rcs $@ $(filter %.o, $^)

build/%.o: src/%.c $(INCLUDE_FILES)
	$Q $(MKDIR) -p $(dir $@)
	$(call cmd,CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

includes/db-spec.h: includes/db-api/encode.h includes/db-api/decode.h

clean:
	$(RM) -rf $(TGT) build tags includes/db-api \
            src/db-api_encode.c src/db-api_decode.c


include $(TOP)/protocols/Rules.mk
$(eval $(call MAKE_PROTOCOL,db-api,includes,src))




