# Makefile for preparation of complex TEX documents:
# make tex, make dvi, make ps, make print, make clean
# =========================================================================

ROOT=root
DOCUMENT=manual
LOCAL_DICT=/clients/sali/dbase/ref/dict.local
SOURCES=macros.tex defs.tex top_incl.tex title.tex \
	../INSTALLATION asgl_incl.tex ../examples/examples_incl.tex
LOCAL_DICT=/clients/sali/dbase/ref/dict.local

# =========================================================================
# ======================== End of usual modifications =====================
# =========================================================================


SHELL=/bin/sh

.IGNORE:

default: dvi

manual: $(SOURCES)
	(cd ../examples; make ps_files)
	collect ${ROOT}.tex perl.tex
	./options.pl perl.tex > ${DOCUMENT}.tex ; rm perl.tex

dvi: $(SOURCES)
	(cd ../examples; make ps_files)
	collect ${ROOT}.tex perl.tex
	./options.pl perl.tex > ${DOCUMENT}.tex ; rm perl.tex
	latex $(DOCUMENT)
#	bibtex $(DOCUMENT)
	makeindex $(DOCUMENT).idx
	latex $(DOCUMENT)
	latex $(DOCUMENT)

html:
	latex2html -split 3 -link 5 -init_file latex2html-init \
		-t "ASGL 1.2 manual" ${DOCUMENT}.tex

gs:
	gs ${DOCUMENT}.ps

ps: 
	dvips $(DOCUMENT) -o $(DOCUMENT).ps

print:
	lpr -h "${DOCUMENT}.ps"

clean:
	rm *.dvi *.log *.aux *.toc *.bbl *.tof *.lof *.lot *.ana *.lot
	rm *.idx *.ind *.ilg *.blg *~ #*# .*~

distclean: clean
	rm ${DOCUMENT}.tex ${DOCUMENT}.ps
	rm -r ${DOCUMENT}

cleanfigs:

tar: clean
	(cd .. ; tar cvf ${DOCUMENT}.tar ./${DOCUMENT})
	(cd .. ; compress -f ${DOCUMENT}.tar; ls -lsa ${DOCUMENT}.tar.Z)

spell: $(SOURCES)
	ispell -W 2 -S -m -B -t -b $(SOURCES)
#       detex ${DOCUMENT}.tex | spell -b +${LOCAL_DICT} > spell.log

# -W 2 ... 2 char or shorter always OK
# -S do not sort alternatives
# -m make root/affix combinations that are not in a dictionary
# -B when no blank between two words, it is an error
# -t LaTeX/TeX
# -b create a backup file
