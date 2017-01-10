.PHONY: pdf build clean

DOC_DIR=dist/doc/html/BaseHashDSL
DOC_FILE:=BaseHashDSL-$(shell cat BaseHashDSL.cabal | grep '^Version:' | awk '{print $$2}')

build:
	cabal install --enable-documentation

pdf: BaseHashDSL.pdf

BaseHashDSL.pdf::
	cabal configure
	cabal haddock --haddock-option=--latex
	cd $(DOC_DIR) && pdflatex $(DOC_FILE).tex
	cp $(DOC_DIR)/$(DOC_FILE).pdf $@


clean:
	cabal clean
	-rm BaseHashDSL.pdf

