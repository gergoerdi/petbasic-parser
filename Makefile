IDRIS2	= $(HOME)/.idris2/bin/idris2

.PHONY: all
all: html/pokol.js

.PHONY: build/exec/pokol.js
build/exec/pokol.js:
	$(IDRIS2) --build pokol.ipkg

html/pokol.js: build/exec/pokol.js
	cp -f $< $@
