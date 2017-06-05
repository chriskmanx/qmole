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
#  ATS is  free software;  you can redistribute it and/or modify it under
#  the  terms of the  GNU General Public License as published by the Free
#  Software Foundation; either version 2.1, or (at your option) any later
#  version.
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
#
# A simple example of using [atslex]
#
######

ATSCC=$(ATSHOME)/bin/atscc
ATSLEX=$(ATSHOME)/bin/atslex

######

calc: calc_lats_dats.c; $(ATSCC) -g -o calc $< -lats_lex

######

calc_lats.dats: calc.lats; $(ATSLEX) < $< > $@
calc_lats_dats.c: calc_lats.dats; $(ATSCC) -c calc_lats.dats

######

clean::
	rm -f *~
	rm -f calc_lats.dats calc_lats_dats.c calc_lats_dats.o

cleanall: clean
	rm -f ./calc

###### end of  [Makefile] ######
