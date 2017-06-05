#
# Atol file manager plugin (Linux/Gtk makefile)
#
# USAGE:
# 1. type "make" to create program binary
#

# check these defines

#
# other settings
#

#DEBUG=-g
CXX=gcc
CFLAGS= -I ../
FLAGS= $(CFLAGS)

# define compile/link command line with flags
CC=$(CXX) $(DEBUG) -fPIC -O2 -MMD -pthread -Wall -D_REENTRANT
LD=$(CXX) $(DEBUG) -fPIC

#
# define default project to build
#
.PHONY: all

all: bzlib

#
# list of all objects to build (atol dependency)
#

SRCPATH=.
BZ2SRC=./bzip2

OBJS= Bz2Lib.o ArchiveInfo.o bz2.o $(BZ2SRC)/libbz2.a

# link program binary
# mark symbols with dllexport in source, or try -Wl,--export-all
bzlib: $(OBJS)
	$(LD) $(OBJS) -o Bz2Lib.atp $(FLAGS) $(LDFLAGS) -shared -fpic --export-all --enable-auto-import

# compile individual objects
# TOFIX?: use pattern rules (how to adjust for possible multiple source directories?)

Bz2Lib.o: $(SRCPATH)/Bz2Lib.cpp
	$(CC) $(FLAGS) -c $(SRCPATH)/Bz2Lib.cpp -o Bz2Lib.o

ArchiveInfo.o: $(SRCPATH)/ArchiveInfo.cpp $(SRCPATH)/ArchiveInfo.h
	$(CC) $(FLAGS) -c $(SRCPATH)/ArchiveInfo.cpp -o ArchiveInfo.o

bz2.o: $(SRCPATH)/bz2.cpp $(SRCPATH)/bz2.h
	$(CC) $(FLAGS) -c $(SRCPATH)/bz2.cpp -o bz2.o

#
# clean up the mess
#
.PHONY : clean

clean:
	@echo cleaning up
	rm -f *.o *.d *.atp

#
# help printout
#
.PHONY : help

help:
	@echo Usage:
	@echo make           - builds application binaries
	@echo make help      - prints this help

