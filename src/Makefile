##############################################################################
#
# This Makefile relies on the environment variable ASGL_EXECUTABLE_TYPE
# to select the proper compiler and compiler flags.
#
##############################################################################
#
# The only Installation definitions:
#
# The directory where the binary will be installed (to be put on your path):
BINDIR=$${ASGLINSTALL}
# BINDIR=/usr/local/bin
#
# The directory where the libraires for running the program will be installed
LIBDIR=$${ASGLINSTALL}/libasgl
# LIBDIR=/usr/local/bin/libasgl
#
# The name of the script for running this program:
EXECSCRIPT=asgl
#
# Files to be installed into the LIB directory:
LIBFILES=top.ini psgl.ini ../data/egromos.vdw ../data/egromos.brk ../data/3d.lib
#
# Files to be installed into the BIN directory:
BINFILES=../scripts/__asgl.top ../scripts/setasgl
# 
# The directory where the include files for compilation are:
INCLUDE=-I../include
#
# A modified fmakedepend that understands CPP-style #include directives:
FMAKEDEPEND=../scripts/fmakedepend -I ../include
MKDIRS=../scripts/mkdirs
#
#
# Program name:
PROGRAM=asgl
#
# Source files for the program:
FILES=top.F top_misc.F top_act.F molrout.F \
	head.F psgl.F asgl.F molec.F cs.F ../lib/lsq_lib.F \
	../lib/io.F ../lib/string.F ../lib/readbrk.F ../lib/nr.F \
	../lib/residues.F ../lib/readlib.F ../lib/ratype.F \
	../lib/degrad.F ../lib/geom1.F ../lib/orthog.F ../lib/sys.F \
	../lib/matrix.F ../lib/wrtmsg.F

include ../scripts/Makefile.include1

# DO NOT DELETE THIS LINE - used by make depend
../lib/degrad.OBJEXT: ../include/numbers.cst ../include/numbers_double.cst ../include/numbers_single.cst 
../lib/geom1.OBJEXT: ../include/numbers.cst ../include/numbers_double.cst ../include/numbers_single.cst 
../lib/io.OBJEXT: ../include/lenf.cst 
../lib/orthog.OBJEXT: ../include/io.cst 
asgl.OBJEXT: ../include/psgl.cmn ../include/psgl.cst ../include/asgl.cst ../include/asgl.cmn ../include/cs.cmn ../include/top_appl.cmn ../include/molec.cmn ../include/molec.cst ../include/top.cmn ../include/top.cst ../include/io.cst 
cs.OBJEXT: ../include/asgl.cmn ../include/psgl.cmn ../include/asgl.cst ../include/molec.cmn ../include/cs.cmn ../include/molec.cst ../include/psgl.cst 
head.OBJEXT: ../include/asgl.cst 
molec.OBJEXT: ../include/top.cmn ../include/top.cst ../include/io.cst 
psgl.OBJEXT: ../include/psgl.cmn ../include/asgl.cst ../include/io.cst ../include/psgl.cst 
top.OBJEXT: ../include/top_appl.cmn ../include/top.cmn ../include/top.cst ../include/io.cst ../include/asgl.cmn ../include/psgl.cmn ../include/asgl.cst ../include/molec.cmn ../include/cs.cmn ../include/psgl.cst ../include/molec.cst 
top_act.OBJEXT: ../include/top_appl.cmn ../include/top.cmn ../include/asgl.cmn ../include/top.cst ../include/io.cst ../include/psgl.cmn ../include/asgl.cst ../include/molec.cmn ../include/cs.cmn ../include/psgl.cst ../include/molec.cst 
top_misc.OBJEXT: ../include/top_appl.cmn ../include/top.cmn ../include/asgl.cmn ../include/psgl.cmn ../include/asgl.cst ../include/molec.cmn ../include/cs.cmn ../include/psgl.cst ../include/molec.cst ../include/top.cst ../include/io.cst 
