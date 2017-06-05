# Makefile for gperf/lib

# Copyright (C) 1989, 1992, 1993, 1998, 2002 Free Software Foundation, Inc.
# Written by Douglas C. Schmidt <schmidt@ics.uci.edu>
# and Bruno Haible <bruno@clisp.org>.
#
# This file is part of GNU GPERF.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#### Start of system configuration section. ####

# Directories used by "make":
srcdir = .

# Directories used by "make install":
prefix = /usr/local
local_prefix = /usr/local
exec_prefix = ${prefix}

# Programs used by "make":
# C compiler
CC = /usr/bin/gcc
CFLAGS = -g -O2
CPP = /usr/bin/gcc -E
# C++ compiler
CXX = g++
CXXFLAGS = -g -O2
CXXCPP = g++ -E
# Both C and C++ compiler
OBJEXT = o
# Other
AR = ar
AR_FLAGS = rc
RANLIB = ranlib
MV = mv
LN = ln
RM = rm -f


# Programs used by "make install":
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644

#### End of system configuration section. ####

SHELL = /bin/sh



OBJECTS  = getopt.$(OBJEXT) getopt1.$(OBJEXT) getline.$(OBJEXT) hash.$(OBJEXT)
CPPFLAGS = -I$(srcdir)

TARGETLIB = libgp.a

all : $(TARGETLIB)

$(TARGETLIB): $(OBJECTS)
	$(RM) $@
	$(AR) $(AR_FLAGS) $@ $(OBJECTS)
	$(RANLIB) $@

# Don't use implicit rules, since AIX "make" and OSF/1 "make" don't always
# expand $< correctly in this context.
#
#%.o : %.c
#	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<
#
#%.o : %.cc
#	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $<

getopt.o : getopt.c getopt.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(srcdir)/getopt.c

getopt1.o : getopt1.c getopt.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(srcdir)/getopt1.c

getline.o : getline.cc getline.h
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $(srcdir)/getline.cc

hash.o : hash.cc hash.h
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $(srcdir)/hash.cc

install : all

installdirs :

uninstall :

check : all

mostlyclean : clean

clean : force
	$(RM) *~ *.s *.o *.a $(TARGETLIB) core

distclean : clean
	$(RM) config.status config.log config.cache Makefile

maintainer-clean : distclean

force :
