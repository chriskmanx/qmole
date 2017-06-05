#########################################################################
##                                                                     ##
##                         Applied Type System                         ##
##                                                                     ##
##                              Hongwei Xi                             ##
##                                                                     ##
#########################################################################

##
## ATS/Anairiats - Unleashing the Power of Types!
##
## Copyright (C) 2002-2008 Hongwei Xi, Boston University
##
## All rights reserved
##
## ATS is  free software;  you can redistribute it and/or modify it under
## the  terms of the  GNU General Public License as published by the Free
## Software Foundation; either version 2.1, or (at your option) any later
## version.
## 
## ATS is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
## for more details.
## 
## You  should  have  received  a  copy of the GNU General Public License
## along  with  ATS;  see the  file COPYING.  If not, please write to the
## Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.
##

## ###### ###### ##

## Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
## Time: June 2008

## ###### ###### ##

GCC=gcc
ATSCC="${ATSHOME}"/bin/atscc

#CCFLAG=
CCFLAG=-O2
#ATSCCFLAG=-g
#ATSCCFLAG=-DGCATS_DEBUG
#ATSCCFLAG=-O2
ATSCCFLAG=-O2 -g

######

SOURCES := \
  gcats1_misc.dats \
  gcats1_freeitmlst.dats \
  gcats1_chunk.dats \
  gcats1_globalentry.dats \
  gcats1_marking.dats \
  gcats1_collecting.dats \
  gcats1_autops.dats \
  gcats1_manops.dats \
  gcats1_top.dats \

TARGETS := $(patsubst %.dats, %_dats.c, $(SOURCES))
TARGETS_mt := $(TARGETS) gcats1_multithread_dats.c
OBJECTS := $(patsubst %.dats, %_dats.o, $(SOURCES))
OBJECTS_mt := $(OBJECTS) gc_multithread_dats.o

######

#
# this generates a slower version:
#
# gc.o: gcats1_sats.o $(OBJECTS)
#	ld -r -o gc.o gcats1_sats.o $(OBJECTS)
#

gc.o: gc0_hats gcats1_sats.o gcats1_all_cats.o
	ld -r -o $@ gcats1_sats.o gcats1_all_cats.o

gc_mt.o: gc1_hats gcats1_sats.o gcats1_mt_all_cats.o
	ld -r -o $@ gcats1_sats.o gcats1_mt_all_cats.o

######

#
# mainly for the purpose of debugging
#
gcats1_main: gc0_hats gc.cats gcats1_sats.c gcats1_main.dats $(OBJECTS)
	$(ATSCC) $(CCFLAG) -o gcats1_main gcats1_main.dats gcats1_sats.c $(OBJECTS)

gcats1_main_mt: gc1_hats gc.cats gcats1_sats.c gcats1_main.dats $(OBJECTS_mt)
	$(ATSCC) $(CCFLAG) -pthread -o gcats1_main_mt gcats1_main.dats gcats1_sats.c $(OBJECTS_mt)

######

gc0_hats: ; gcc -E -P -D_ATS_MULTITHREAD0 -x c gcats1_master.hats -o gcats1.hats
gc1_hats: ; gcc -E -P -D_ATS_MULTITHREAD1 -x c gcats1_master.hats -o gcats1.hats

######

#
# this kind of whole compilation can probably take advantage of
# interprocedural optimization:
#
gcats1_all_cats.o: gcats1_sats.c $(TARGETS)
	$(ATSCC) $(CCFLAG) -o gcats1_all_cats.o -c -x c gcats1_all.cats

gcats1_mt_all_cats.o: gcats1_sats.c $(TARGETS_mt)
	$(ATSCC) $(CCFLAG) -o gcats1_mt_all_cats.o -c -x c gcats1_all.cats

######

gcats1_sats.c: gcats1.sats
	$(ATSCC) $(ATSCCFLAG) -cc gcats1.sats
gcats1_sats.o: gcats1_sats.c; $(ATSCC) $(CCFLAG) -c gcats1_sats.c

######

gcats1_top_dats.c: gcats1_top.dats
	$(ATSCC) $(ATSCCFLAG) -cc gcats1_top.dats
gcats1_top_dats.o: gcats1_top_dats.c; $(ATSCC) $(CCFLAG) -c gcats1_top_dats.c

gcats1_misc_dats.c: gcats1_misc.dats
	$(ATSCC) $(ATSCCFLAG) -cc gcats1_misc.dats
gcats1_misc_dats.o: gcats1_misc_dats.c
	$(ATSCC) $(CCFLAG) -c gcats1_misc_dats.c

gcats1_freeitmlst_dats.c: gcats1_freeitmlst.dats
	$(ATSCC) $(ATSCCFLAG) -cc gcats1_freeitmlst.dats
gcats1_freeitmlst_dats.o: gcats1_freeitmlst_dats.c
	$(ATSCC) $(CCFLAG) -c gcats1_freeitmlst_dats.c

gcats1_chunk_dats.c: gcats1_chunk.dats
	$(ATSCC) $(ATSCCFLAG) -cc gcats1_chunk.dats
gcats1_chunk_dats.o: gcats1_chunk_dats.c
	$(ATSCC) $(CCFLAG) -c gcats1_chunk_dats.c

gcats1_globalentry_dats.c: gcats1_globalentry.dats
	$(ATSCC) $(ATSCCFLAG) -cc gcats1_globalentry.dats
gcats1_globalentry_dats.o: gcats1_globalentry_dats.c
	$(ATSCC) $(CCFLAG) -c gcats1_globalentry_dats.c

gcats1_multithread_dats.c: gcats1_multithread.dats
	$(ATSCC) $(ATSCCFLAG) -cc gcats1_multithread.dats
gcats1_multithread_dats.o: gcats1_multithread_dats.c
	$(ATSCC) $(CCFLAG) -c gcats1_multithread_dats.c

gcats1_marking_dats.c: gcats1_marking.dats
	$(ATSCC) $(ATSCCFLAG) -cc gcats1_marking.dats
gcats1_marking_dats.o: gcats1_marking_dats.c
	$(ATSCC) $(CCFLAG) -c gcats1_marking_dats.c

gcats1_collecting_dats.c: gcats1_collecting.dats
	$(ATSCC) $(ATSCCFLAG) -cc gcats1_collecting.dats
gcats1_collecting_dats.o: gcats1_collecting_dats.c
	$(ATSCC) $(CCFLAG) -c gcats1_collecting_dats.c

gcats1_autops_dats.c: gcats1_autops.dats
	$(ATSCC) $(ATSCCFLAG) -cc gcats1_autops.dats
gcats1_autops_dats.o: gcats1_autops_dats.c
	$(ATSCC) $(CCFLAG) -c gcats1_autops_dats.c

gcats1_manops_dats.c: gcats1_manops.dats
	$(ATSCC) $(ATSCCFLAG) -cc gcats1_manops.dats
gcats1_manops_dats.o: gcats1_manops_dats.c
	$(ATSCC) $(CCFLAG) -c gcats1_manops_dats.c

######

srclines::
	wc -l *.?ats

######

clean::
	rm -f *~
	rm -f *_?ats.c *_?ats.o
	rm -f gcats1.hats

cleanall:: clean; rm -f gc.o gc_mt.o

######
#
# end of [Makefile]
#
######
