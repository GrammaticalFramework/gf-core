gf-help-full.txt::
	{ echo ; echo ; echo ; } > $@
	echo help -full -t2t | gf -run >> $@

### Converting SVG logos to PNG, using convert from ImageMagick
logos:: Logos/gf00.png Logos/gf0.png

Logos/gf00.png: Logos/gf00.svg Makefile
	convert -background 'rgba(255,255,255,0)' -size 264x216 -depth 8 $< $@

Logos/gf0.png: Logos/gf0.svg Makefile
	convert -background 'rgba(255,255,255,0)' -size 320x240 -depth 8 $< $@
