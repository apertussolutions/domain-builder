TOP=..
include $(TOP)/mk/config

TGT           =control

DB_SERV_PATH  = $(TOP)/db-server
LIBC          = $(TOP)/lib/c
LIBSMALLOS    = $(TOP)/lib/smallOS
LIBCHAN       = $(TOP)/lib/chan
LIBSHA256     = $(TOP)/lib/sha256
LIBDBSPEC     = $(TOP)/lib/db-spec
LIBCPIO       = $(TOP)/lib/cpio
DM_LAUNCHER   = $(TOP)/tools/dm-launcher

LIB_DIRS      =$(LIBC) $(LIBSMALLOS) $(LIBCHAN) $(LIBDBSPEC)
CFLAGS       +=-std=gnu99
INCLUDE_DIRS  =include \
               $(LIBC)/includes \
               $(LIBSMALLOS)/includes \
               $(XEN_HEADERS) \
               $(LIBCHAN)/includes \
               $(LIBDBSPEC)/includes \
               $(DB_SERV_PATH)/includes \
               $(LIBSHA256)/includes \
               $(LIBCPIO)/includes \
               $(DM_LAUNCHER)/includes \

.PHONY: default
default: $(TGT)

include $(TOP)/mk/rules
include $(TOP)/protocols/Rules.mk
$(eval $(call MAKE_PROTOCOL,control-domain-api,include,protocols))
$(eval $(call MAKE_PROTOCOL,control-domain-schema,include,protocols))

C_FILES=protocols/control-domain-api_decode.c \
        protocols/control-domain-api_encode.c \
        protocols/control-domain-schema_decode.c \
        $(wildcard *.c)

$(TGT): $(patsubst %.c,%.o,$(C_FILES)) smallOS_compat.o \
      $(LIBCHAN)/libchan.a \
      $(LIBDBSPEC)/libdb-spec.a \
      $(LIBSHA256)/libsha256.a \
      $(LIBSMALLOS)/libsmallOS.a \
      $(LIBCPIO)/libcpio.a \
      $(LIBC)/libc.a \
      $(INCLUDE_FILES)
	$(call cmd,LD) $(LDFLAGS) \
              $(filter %.o, $^) \
              $(patsubst lib%.a, -l%, $(filter %.a, $^)) \
              -o $@

control-domain.pdf: control-domain.markdown
	$(PANDOC) -o $@ $^

clean: basic_clean
	rm -rf protocols include/control-domain-api include/control-domain-schema
