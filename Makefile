#
# Before using this makefile, be sure to set
# - ASGLINSTALL: to the directory where ASGL is to be installed
# - ASGL_EXECUTABLE_TYPE: to the buildtype of your machine
#                (e.g. g77, iris4d, next - see scripts/Makefile.include1)
#

#############################################################################
# Installation definitions:
# The directory where the libraires for running the program will be installed
LIBDIR=$${ASGLINSTALL}/libasgl
# LIBDIR=/usr/local/bin/libasgl
#############################################################################

# Top Makefile for ASGL:
#
# make all        ... create all components (default)
# make install    ... install all components
# make man        ... make a PS document for ASGL (requires asgl in the path)
# make machines   ... make and install everything on all MK machines
# make clean      ... to rm garbage from the program directories
# make distclean  ... to rm everything in all directories except the sources
# make deb        ... to compile with all debugging switches ON
# make opt        ... to compile with full optimization
# make makefiles  ... to remake Makefiles
# make smalldist  ... to prepare a small distribution file
# make dist       ... to prepare a full distribution file
# make floppy     ... puts tar file from 'make dist' on /DOS floppy (NeXT only)
# make implicitsun... to change to the SUN's 'implicit undefined (a-z)'
# make implicitvms... to change to the VMS's 'implicit none'
# make installman ... make and install a PS document for ASGL
# make installtop ... install example top files
#
#

SHELL=/bin/sh

PROG_DIRS=./src
SUPP_DIRS=./doc ./lib ./scripts ./examples ./data ./notes
FILES=./Makefile ./README ./INSTALLATION
PACKAGE=asgl

SMALLDISTRIBUTION=./asgl
# SMALLDISTRIBUTION=${PROG_DIRS} ${FILES} ${SUPP_DIRS}
DISTRIBUTION=${SMALLDISTRIBUTION}

.IGNORE:

default: all

all: opt

installall: all install installtop installman

opt:
	for DIR in ${PROG_DIRS} ; do (cd $${DIR} ; make opt); done

deb:
	for DIR in ${PROG_DIRS} ; do (cd $${DIR} ; make deb); done

clean:
	for DIR in ${PROG_DIRS} ; do (cd $${DIR} ; echo `pwd`; make clean); done
	rm *.log

distclean: clean
	for DIR in ${PROG_DIRS} ; do (cd $${DIR} ; make distclean); done
	for DIR in ${SUPP_DIRS} ; do (cd $${DIR} ; make distclean); done
	rm *.tar.Z

makefiles:
	for DIR in ${PROG_DIRS} ; do (cd $${DIR} ; make_mf); done

smalldist: distclean
	(cd ../; tar cvf ${PACKAGE}.tar ${SMALLDISTRIBUTION}; \
		mv ${PACKAGE}.tar asgl)
	compress -f ${PACKAGE}.tar
	ls -ls ${PACKAGE}.tar.Z

dist: distclean
	(cd ../; tar cvf ${PACKAGE}.tar ${DISTRIBUTION} ; \
		mv ${PACKAGE}.tar asgl)
	compress -f ${PACKAGE}.tar
	ls -ls ${PACKAGE}.tar.Z

floppy: dist
	cp ${PACKAGE}.tar.Z /DOS/${PACKAGE}.TRZ
	ls -ls /DOS

implicitsun:
	for DIR in ${PROG_DIRS} ; do (cd $${DIR} ; implicit sun); done	

implicitvms:
	for DIR in ${PROG_DIRS} ; do (cd $${DIR} ; implicit vms); done	

man: 
	(cd doc; make ps)

install:
	(cd src; make install)

installman: man
	if [ ! -d ${LIBDIR} ] ; then mkdir ${LIBDIR} ; fi
	(cd doc; cp manual.ps ${LIBDIR})

installtop:
	if [ ! -d ${LIBDIR} ] ; then mkdir ${LIBDIR} ; fi
	(cd examples; cp *.top *.dat ${LIBDIR})

machines:
	make distclean
	DIR=~/data/asgl; \
#	rsh mischa  -n "cd $${DIR}; \
#	ASGLBIN=~/data/bin/scripts ; ASGLLIB=~/data/bin/scripts/asgllib ;  \
#	export ASGLBIN ASGLLIB ; \
#	make install ; make distclean" ; \
#	rsh diamond -n "cd $${DIR}; \
#	ASGLBIN=~/data/bin/scripts ; ASGLLIB=~/data/bin/scripts/asgllib ;  \
#	export ASGLBIN ASGLLIB ; \
#	make install ; make distclean" ; \
	rsh proline -n "cd $${DIR}; \
	ASGLBIN=~/data/bin/scripts ; ASGLLIB=~/data/bin/scripts/asgllib ;  \
	export ASGLBIN ASGLLIB ; \
	make install; make distclean" ; \
#	rsh tammy   -n "cd $${DIR}; \
#	ASGLBIN=~/data/bin/scripts ; ASGLLIB=~/data/bin/scripts/asgllib ;  \
#	export ASGLBIN ASGLLIB ; \
#	make installall ; make distclean"
