# BANNERSTART
# Copyright: 2011 Galois Inc.
# License-file: LICENSE
# BANNEREND

TOP=../..
include $(TOP)/mk/config

TGT     = libelf-xen.a
all: $(TGT)


LIBC     =../c
SMALLOS  =../smallOS

INCLUDE_DIRS=includes $(SMALLOS)/includes $(LIBC)/includes $(XEN_HEADERS)

include $(TOP)/mk/compile

SRC     = $(wildcard src/*.c)
OBJ     = $(addprefix $(BUILD)/,$(addsuffix .o,$(basename $(notdir $(SRC)))))

BUILD   = build


$(TGT): $(OBJ)
	$(call cmd,AR) rcs $@ $(OBJ)

$(BUILD)/%.o: src/%.c $(INCLUDE_FILES)
	$Q $(MKDIR) -p $(BUILD)
	$(call cmd,CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

.PHONY: all clean cbmc

CBMCINC =-Iincludes -I$(SMALLOS)/includes -I$(LIBC)/includes -I$(XEN_HEADERS)
cbmc:
# libelf-loader.c
	$(call CBMC,src/libelf-loader.c,$(CBMCINC) \
	  --unwind 6, elf_init)
	$(call CBMC,src/libelf-loader.c,$(CBMCINC) \
	  --unwind 6 , elf_set_verbose)
	$(call CBMC,src/libelf-loader.c,$(CBMCINC) \
	  --unwind 6 , elf_parse_bsdsyms)
# broken under cbmc 3.9
# 	$(call CBMC,src/libelf-loader.c,$(CBMCINC) \
# 	  --unwind 6 , elf_load_bsdsyms)
# 	$(call CBMC,src/libelf-loader.c,$(CBMCINC) \
# 	  --unwind 6 , elf_parse_binary)
# 	$(call CBMC,src/libelf-loader.c,$(CBMCINC) \
# 	  --unwind 6 , elf_load_binary)
	$(call CBMC,src/libelf-loader.c,$(CBMCINC) \
	  --unwind 6 , elf_lookup_addr)


clean:
	-$(RM) -rf $(TGT) $(BUILD)


