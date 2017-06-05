#########################################################################
#                                                                       #
#                         Applied Type System                           #
#                                                                       #
#                              Hongwei Xi                               #
#                                                                       #
#########################################################################

#
#  ATS - Unleashing the Power of Types!
#
#  Copyright (C) 2002-2007 Hongwei Xi.
#
#  ATS is free software;  you can  redistribute it and/or modify it under
#  the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
#  Free Software Foundation; either version 2.1, or (at your option)  any
#  later version.
# 
#  ATS is distributed in the hope that it will be useful, but WITHOUT ANY
#  WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
#  for more details.
# 
#  You  should  have  received  a  copy of the GNU General Public License
#  along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
#  Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
#  02110-1301, USA.
#

#
# Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
#

######

ATSHOMEQ="$(ATSHOME)"
ATSCC=$(ATSHOMEQ)/bin/atscc
#ATSCCFLAGS=-D_ATS_gc
#ATSCCFLAGS=-D_ATS_gcats
#ATSCCFLAGS=-D_ATS_GCATS
ATSCCFLAGS=
ATSRUNTIME=$(ATSHOMEQ)/ccomp/runtime

CCFLAGS=-g -O2
INCLUDES=-I$(ATSHOMEQ) -I$(ATSRUNTIME)

######

atslex: \
  atslex.dats top_sats.o lexgen_dats.o \
  charset_dats.o intset_dats.o states_dats.o \
  parser_dats.o position_dats.o token_dats.o
	$(ATSCC) $(ATSCCFLAGS) -o atslex \
  atslex.dats top_sats.o lexgen_dats.o \
  charset_dats.o intset_dats.o states_dats.o \
  parser_dats.o position_dats.o token_dats.o

######

charset_dats.o: charset.dats
	$(ATSCC) $(CCFLAGS) -c charset.dats
intset_dats.o: intset.dats
	$(ATSCC) $(CCFLAGS) -c intset.dats
lexgen_dats.o: lexgen.dats
	$(ATSCC) $(CCFLAGS) -c lexgen.dats
parser_dats.o: parser.dats
	$(ATSCC) $(CCFLAGS) -c parser.dats
position_dats.o: position.dats
	$(ATSCC) $(CCFLAGS) -c position.dats
states_dats.o: states.dats
	$(ATSCC) $(CCFLAGS) -c states.dats
token_dats.o: token.dats
	$(ATSCC) $(CCFLAGS) -c token.dats
top_sats.o: top.sats
	$(ATSCC) $(CCFLAGS) -c top.sats

######

clean::
	rm -f *_sats.c *_dats.c *.o *~

cleanall: clean; rm -f atslex

###### end of [Makefile] ######
