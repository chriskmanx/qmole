## Process this file with automake to produce Makefile.in

###############################################################################
##                                                                           ##
## Copyright (c) 2001 Morgan Stanley Dean Witter & Co. All rights reserved.     ##
## See .../src/LICENSE for terms of distribution.                           ##
##                                                                           ##
##                                                                           ##
###############################################################################

lib_LTLIBRARIES = liba.la

liba_la_SOURCES = \
	att.c \
	beam.c \
	beamconv.c \
	bstub.c \
	callouts.c \
	dbg.c \
	dot.c \
	f.c \
	gpick.c \
	i.c \
	j.c \
	k.c \
	m.c \
	n.c \
	nsf.c \
	o.c \
	ovli.c \
	p.c \
	q.c \
	r.c \
	s.c \
	si.c \
	u.c \
	tmv_nopipe.c \
	y.c \
	arthur.h\
	b.h\
	beam.h\
	development.h\
	f.h\
	fir.h\
	fncdcls.h\
	ik.h\
	k.h\
	pwr2hpc.h\
	s.h\
	x.h


CFLAGS = @CFLAGS@ -DBSTUB

INCLUDES = -I..

a_includedir = $(prefix)/include/a
a_include_DATA = \
	beam.h \
	fir.h \
	fncdcls.h \
	k.h 
