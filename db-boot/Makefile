TOP=..
include $(TOP)/mk/config

TGT=db-boot

.PHONY: default
default: $(TGT)

LIBC        =../lib/c
LIBSMALLOS  =../lib/smallOS
LIBCPIO     =../lib/cpio
LIBELF      =../lib/elf
LIBDB       =../lib/db
LIBDBSPEC   =../lib/db-spec
LIBZ        =../lib/zlib-1.2.5
LIBSHA256   =../lib/sha256

LIB_DIRS    =$(LIBZ) $(LIBC) $(LIBCPIO) $(LIBSMALLOS) $(LIBELF) \
            $(LIBDB) $(LIBSHA256) $(LIBDBSPEC)

INCLUDE_DIRS = \
             $(LIBC)/includes \
             $(LIBCPIO)/includes \
             $(LIBSMALLOS)/includes \
             $(LIBELF)/includes \
             $(LIBDB)/includes \
             $(LIBDBSPEC)/includes \
             $(LIBZ) \
             $(LIBSHA256)/includes \
             $(XEN_HEADERS)


include $(TOP)/mk/rules

test:
	echo $(INCLUDE_DIRS)

$(TGT): $(patsubst %.c,%.o,$(wildcard *.c)) \
      $(LIBC)/libc.a $(LIBCPIO)/libcpio.a $(LIBSMALLOS)/libsmallOS.a \
      $(LIBZ)/libz.a $(LIBSHA256)/libsha256.a \
      $(LIBDBSPEC)/libdb-spec.a \
      $(LIBELF)/libelf-xen.a $(LIBDB)/libDomainBuilder.a
	$(call cmd,LD) $(LDFLAGS) $(filter %.o, $^) -o $@ \
         -ldb-spec -lz -lsha256 -lDomainBuilder -lelf-xen \
         -lsmallOS -lcpio -lc

clean: basic_clean


.PHONY: cbmc
cbmc:
	cbmc kernel.c -I$(INCLUDE_DIRS) --pointer-check
