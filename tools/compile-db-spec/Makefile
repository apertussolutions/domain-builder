TOP=../..
include $(TOP)/mk/rules-basic
include $(TOP)/protocols/Rules.mk


.PHONY: install clean
install: protocols/control-domain-schema_encode.c
	$Q cabal install --bindir=../../bin

$(eval $(call MAKE_PROTOCOL,control-domain-schema,include,protocols))


clean:
	@cabal clean
	rm -rf include/control-domain-schema protocols


