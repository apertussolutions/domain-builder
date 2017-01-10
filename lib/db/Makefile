TOP=../..
include $(TOP)/mk/config

TGT=libDomainBuilder.a
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


ARCH_HDRS=src/generated/x86_32.h src/generated/x86_64.h
.SECONDARY: $(ARCH_HDRS)

build/dom_mem.o: src/dom_mem.c src/dom_mem.generic.c
build/dom_phys_map.o: src/dom_phys_map.c src/dom_phys_map.generic.c
build/dom_elf.o: src/dom_elf.c src/dom_elf.generic.c
build/dom_builder.o: src/dom_builder.c src/dom_builder.generic.c
build/dom_page_tables.x86_64.o: src/dom_page_tables.x86_64.c \
                                src/dom_page_tables.generic.c
build/dom_page_tables.x86_32pae.o: src/dom_page_tables.x86_32pae.c \
                                   src/dom_page_tables.generic.c


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




