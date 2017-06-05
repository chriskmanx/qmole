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

ATSHOMEQ="$(ATSHOME)"
ATSOPT=$(ATSHOMEQ)/bin/atsopt
ATSRUNTIME=$(ATSHOMEQ)/ccomp/runtime
ATSLIBC=$(ATSHOMEQ)/libc
ATSCC=$(ATSHOMEQ)/bin/atscc

CC=gcc
INCLUDES=-I$(ATSHOMEQ) -I$(ATSRUNTIME)
CCFLAGS=-O2 -g

######

all: atscc atslib
	echo "[atscc] and [atslib] have been generated."

atscc:  atscc_main_dats.c \
          top_sats.o basics_dats.o pkgconfig.o atscc_dats.o \
          errno_sats.o fcntl_sats.o stdio_sats.o stdlib_sats.o string_sats.o unistd_sats.o \
          sys_types_sats.o sys_wait_sats.o
	$(CC) $(INCLUDES) $(CCFLAGS) -o atscc atscc_main_dats.c $(ATSRUNTIME)/ats_prelude.c \
          top_sats.o basics_dats.o pkgconfig.o atscc_dats.o \
          errno_sats.o fcntl_sats.o stdio_sats.o stdlib_sats.o string_sats.o unistd_sats.o \
          sys_types_sats.o sys_wait_sats.o

atslib: atslib_main_dats.c \
          top_sats.o basics_dats.o atscc_dats.o atslib_dats.o \
          fcntl_sats.o stdio_sats.o stdlib_sats.o unistd_sats.o \
          sys_types_sats.o sys_wait_sats.o
	$(CC) $(INCLUDES) $(CCFLAGS) -o atslib atslib_main_dats.c $(ATSRUNTIME)/ats_prelude.c \
          top_sats.o basics_dats.o atscc_dats.o atslib_dats.o \
          fcntl_sats.o stdio_sats.o stdlib_sats.o unistd_sats.o \
          sys_types_sats.o sys_wait_sats.o

atspack: atspack.dats
	$(ATSCC) -o atspack $(CCFLAGS) atspack.dats

deltabs: deltabs.dats
	$(ATSCC) -o deltabs $(CCFLAGS) deltabs.dats -D_ATS_gcats

######

top_sats.c: top.sats
	$(ATSOPT) --output $@ --static top.sats
top_sats.o: top_sats.c
	$(CC) $(INCLUDES) $(CCFLAGS) -o $@ -c top_sats.c

basics_dats.c: top.sats basics.dats
	$(ATSOPT) --output $@ --dynamic basics.dats
basics_dats.o: basics_dats.c
	$(CC) $(INCLUDES) $(CCFLAGS) -o $@ -c basics_dats.c

######

pkgconfig_dats.c: pkgconfig.dats
	$(ATSOPT) --output $@ --dynamic pkgconfig.dats

pkgconfig.o: pkgconfig_dats.c
	$(CC) $(INCLUDES) $(CCFLAGS) -o $@ -c pkgconfig_dats.c

######

atscc_dats.c: top.sats atscc.dats
	$(ATSOPT) --output $@ --dynamic atscc.dats
atscc_dats.o: atscc_dats.c
	$(CC) $(INCLUDES) $(CCFLAGS) -o $@ -c atscc_dats.c

atscc_main_dats.c: atscc_main.dats
	$(ATSOPT) --output $@ --dynamic atscc_main.dats

######

atslib_dats.c: top.sats atslib.dats
	$(ATSOPT) --output $@ --dynamic atslib.dats
atslib_dats.o: atslib_dats.c
	$(CC) $(INCLUDES) $(CCFLAGS) -o $@ -c atslib_dats.c

atslib_main_dats.c: atslib_main.dats
	$(ATSOPT) --output $@ --dynamic atslib_main.dats

######

errno_sats.c:
	$(ATSOPT) --output $@ --static $(ATSLIBC)/SATS/errno.sats
errno_sats.o: errno_sats.c
	$(CC) $(INCLUDES) $(CCFLAGS) -o $@ -c errno_sats.c

fcntl_sats.c:
	$(ATSOPT) --output $@ --static $(ATSLIBC)/SATS/fcntl.sats
fcntl_sats.o: fcntl_sats.c
	$(CC) $(INCLUDES) $(CCFLAGS) -o $@ -c fcntl_sats.c

stdio_sats.c:
	$(ATSOPT) --output $@ --static $(ATSLIBC)/SATS/stdio.sats
stdio_sats.o: stdio_sats.c
	$(CC) $(INCLUDES) $(CCFLAGS) -o $@ -c stdio_sats.c

stdlib_sats.c:
	$(ATSOPT) --output $@ --static $(ATSLIBC)/SATS/stdlib.sats
stdlib_sats.o: stdlib_sats.c
	$(CC) $(INCLUDES) $(CCFLAGS) -o $@ -c stdlib_sats.c

string_sats.c:
	$(ATSOPT) --output $@ --static $(ATSLIBC)/SATS/string.sats
string_sats.o: string_sats.c
	$(CC) $(INCLUDES) $(CCFLAGS) -o $@ -c string_sats.c

unistd_sats.c:
	$(ATSOPT) --output $@ --static $(ATSLIBC)/SATS/unistd.sats
unistd_sats.o: unistd_sats.c
	$(CC) $(INCLUDES) $(CCFLAGS) -o $@ -c unistd_sats.c

sys_types_sats.c:
	$(ATSOPT) --output $@ --static $(ATSLIBC)/sys/SATS/types.sats
sys_types_sats.o: sys_types_sats.c
	$(CC) $(INCLUDES) $(CCFLAGS) -o $@ -c sys_types_sats.c

sys_wait_sats.c:
	$(ATSOPT) --output $@ --static $(ATSLIBC)/sys/SATS/wait.sats
sys_wait_sats.o: sys_wait_sats.c
	$(CC) $(INCLUDES) $(CCFLAGS) -o $@ -c sys_wait_sats.c

######

clean::
	rm -f *_sats.c *_dats.c *.o *~

cleanall:: clean
	rm -f atscc atslib atspack

######
