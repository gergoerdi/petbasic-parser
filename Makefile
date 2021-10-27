IDRIS2	= $(HOME)/.idris2/bin/idris2

.PHONY: all
all: html/pokol.js html/assets/pics.png

.PHONY: build/exec/pokol.js
build/exec/pokol.js:
	$(IDRIS2) --build pokol.ipkg

html/pokol.js: build/exec/pokol.js
	cp -f $< $@

.PHONY: install
install:
	rsync -Paz html/ ds9.erdi.hu:web/projects/pokol/

html/assets/pics.png: $(wildcard html/assets/pic/*.png)
	montage $^ -tile 1x -geometry +0+0 $@
