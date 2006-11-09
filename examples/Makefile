# Makefile for preparation of complex TEX documents:
# make tex, make dvi, make ps, make print, make clean
# =========================================================================

DOCUMENT=examples

E=../examples

TOPS=$E/scatter.top $E/dplot.top $E/mhist.top $E/error.top $E/bars.top \
	$E/mixed.top $E/1fdx.top $E/3rp2.top $E/types.top $E/pos1.top \
 	$E/pos2.top $E/pos3.top $E/alty.top $E/stkhist.top $E/spectrum.top \
	$E/lattice.top $E/pos0.top $E/labls.top $E/hydr.top $E/cbeta.top \
	$E/fit.top $E/arrow.top
 
PLOTS=$E/scatter.ps $E/dplot.ps $E/mhist.ps $E/error.ps $E/bars.ps \
 	$E/mixed.ps $E/1fdx.ps $E/3rp2.ps $E/types.ps $E/pos1.ps \
 	$E/pos2.ps $E/pos3.ps $E/alty.ps $E/stkhist.ps $E/spectrum.ps \
	$E/lattice.ps $E/pos0.ps $E/labls.ps $E/hydr.ps $E/cbeta.ps \
	$E/fit.ps $E/arrow.ps

SOURCES=examples.tex examples_incl.tex

# =========================================================================
# ======================== End of usual modifications =====================
# =========================================================================

SHELL=/bin/sh

.IGNORE:

.SUFFIXES: .top .ps
.top.ps:
	asgl $<

default: dvi

ps_files: ${PLOTS} $(TOPS)

dvi: ${DOCUMENT}i.tex ${PLOTS}
	latex ${DOCUMENT}i

ps: ${DOCUMENT}i.dvi
	dvips -o ${DOCUMENT}i.ps ${DOCUMENT}i

${DOCUMENT}i.tex: ${SOURCES}
	../doc/collect ${DOCUMENT}.tex ${DOCUMENT}i.tex

print:  ${DOCUMENT}i.ps
	lpr ${DOCUMENT}i.ps
	echo Paper on printer 

clean:
	rm *-t.* *.dvi *.bbl *.aux *.log *.toc *.blg *.tof *.lot *.lof \
	*.ps #*# *.~?~ 
	rm ${DOCUMENT}i.* ${PLOTS}
	rm *.ana *.log

distclean: clean
