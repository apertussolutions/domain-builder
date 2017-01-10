.PHONY: clean all cbmc

O=--no-print-directory

MK=@echo "Building $1"; $(MAKE) $(O) -C $1

all:
	$(call MK,lib/c)
	$(call MK,lib/zlib-1.2.5)
	$(call MK,lib/sha256)
	$(call MK,lib/cpio)
	$(call MK,lib/smallOS)
	$(call MK,lib/console)
	$(call MK,lib/chan)
	$(call MK,lib/elf)
	$(call MK,tools/protocol-spec)
	$(call MK,lib/db-spec)
	$(call MK,lib/db)
	$(call MK,lib/signature)
	$(call MK,tools)
	$(call MK,db-boot)
	$(call MK,control-domain)


clean:
	rm -f bin/*
	@$(MAKE) clean $(O) -C control-domain
	@$(MAKE) clean $(O) -C db-boot
	@$(MAKE) clean $(O) -C lib/signature
	@$(MAKE) clean $(O) -C tools
	@$(MAKE) clean $(O) -C lib/db-spec
	@$(MAKE) clean $(O) -C lib/db
	@$(MAKE) clean $(O) -C lib/elf
	@$(MAKE) clean $(O) -C lib/chan
	@$(MAKE) clean $(O) -C lib/console
	@$(MAKE) clean $(O) -C lib/smallOS
	@$(MAKE) clean $(O) -C lib/cpio
	@$(MAKE) clean $(O) -C lib/sha256
	@$(MAKE) clean $(O) -C lib/zlib-1.2.5
	@$(MAKE) clean $(O) -C lib/c

