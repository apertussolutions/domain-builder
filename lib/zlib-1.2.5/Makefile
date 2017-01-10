TOP=../..
include $(TOP)/mk/config

TGT          =libz.a
all: $(TGT)

LIBC          =../c
INCLUDE_DIRS  =$(LIBC)/includes

include $(TOP)/mk/compile

OBJS          =adler32.o compress.o crc32.o deflate.o infback.o inffast.o inflate.o inftrees.o trees.o uncompr.o zutil.o

$(TGT): $(addprefix build/,$(OBJS))
	$(call cmd,AR) rcs $@ $(filter %.o, $^)

build/%.o: %.c $(INCLUDE_FILES)
	$Q $(MKDIR) -p $(dir $@)
	$(call cmd,CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean:
	$(RM) -rf $(TGT) build


.PHONY: all clean
