.PHONY: install clean


FINDALL=find ./*/ -maxdepth 0 -type d

install:
	make -C protocol-spec
	@$(FINDALL) -printf "Building %f\n" \
                   -exec make --no-print-directory -C {} install \;

clean:
	@$(FINDALL) -exec make --no-print-directory -C {} clean \;

