TOP=../..
include $(TOP)/mk/config

INCLUDE_DIRS  =includes
TGT           =libc.a
all: $(TGT)

include $(TOP)/mk/compile

OBJS=$(patsubst src/%.c,build/%.o, $(wildcard src/*.c))

$(TGT): $(OBJS)
	$(call cmd,AR) rcs $@ $(filter %.o, $^)

build/%.o: src/%.c $(INCLUDE_FILES)
	$Q $(MKDIR) -p $(dir $@)
	$(call cmd,CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean:
	$(RM) -rf $(TGT) build tags


.PHONY: all clean cbmc
