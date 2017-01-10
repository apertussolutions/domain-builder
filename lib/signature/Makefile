TOP=../..
include $(TOP)/mk/config

TGT=libSignature.a
all: $(TGT)

LIBC        =../c
LIBSMALLOS  =../smallOS
LIBELF      =../elf
LIBDBSPEC   =../db-spec

INCLUDE_DIRS=includes $(LIBSMALLOS)/includes $(LIBC)/includes \
            $(LIBELF)/includes $(LIBDBSPEC)/includes $(XEN_HEADERS)

include $(TOP)/mk/compile

OBJS=$(patsubst src/%.c,build/%.o, \
           $(filter-out %.generic.c, $(wildcard src/*.c)))

$(TGT): $(OBJS)
	$(call cmd,AR) rcs $@ $(filter %.o, $^)


build/signature.o : src/signature.c
build/mod_exp.o : src/mod_exp.c

build/%.o: src/%.c $(ARCH_HDRS) $(INCLUDE_FILES)
	$Q $(MKDIR) -p build
	$(call cmd,CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

.PHONY: all clean
clean:
	-$(RM) -rf $(TGT) build src/generated ./build-tools/*.pyc



# ------------------------------------------------------------------------------

PY_TOOLS=./build-tools/mkheader.py ./build-tools/structs.py

src/generated/%.h: $(PY_TOOLS) \
          $(XEN_HEADERS)/xen/arch-x86/xen-%.h \
          $(XEN_HEADERS)/xen/arch-x86/xen.h \
          $(XEN_HEADERS)/xen/xen.h
	$Q $(MKDIR) -p src/generated
	$(call cmd,PYTHON) $< $* $@ $(filter %.h,$^)




