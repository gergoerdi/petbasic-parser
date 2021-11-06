IDRIS2	= $(HOME)/.idris2/bin/idris2
TEXTS	= $(wildcard html/assets/text/*)

.PHONY: all
all: html/pokol.js html/assets/pics.png html/assets/pokol.ppb.js

html/assets/texts.js: build/exec/text $(TEXTS)
	build/exec/text $(TEXTS) > $@

.PHONY: build/exec/pokol.js
build/exec/pokol.js:
	$(IDRIS2) --build pokol.ipkg

.PHONY: build/exec/prepare-binary
build/exec/prepare-binary:
	$(IDRIS2) --build prepare-binary.ipkg

html/assets/pokol.ppb: pokol.mem src/PETBASIC/Binary.idr | build/exec/prepare-binary
	./build/exec/prepare-binary

.PHONY: build/exec/text
build/exec/text:
	$(IDRIS2) --build text.ipkg

html/pokol.js: build/exec/pokol.js
	cp -f $< $@

.PHONY: install
install:
	rsync -Paz html/ ds9.erdi.hu:web/projects/pokol/

html/assets/pics.png: $(wildcard html/assets/pic/*.png)
	montage $^ -tile 1x -geometry +0+0 $@

html/assets/pokol.ppb.js: html/assets/pokol.ppb
	(echo "var ppb64 = \"\\"; \
	base64 $< | sed -e 's/$$/\\/'; \
	echo '";') > $@
