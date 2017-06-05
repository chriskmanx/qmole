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

all: zlib

#
# list of all objects to build (atol dependency)
#

SRCPATH=.

OBJS= ZLib.o ArchiveInfo.o Decompress.o

# link program binary
# mark symbols with dllexport in source, or try -Wl,--export-all
zlib: $(OBJS)
	$(LD) $(OBJS) -o ZLib.atp $(FLAGS) $(LDFLAGS) -shared -fpic --export-all --enable-auto-import

# compile individual objects
# TOFIX?: use pattern rules (how to adjust for possible multiple source directories?)

ZLib.o: $(SRCPATH)/ZLib.cpp
	$(CC) $(FLAGS) -c $(SRCPATH)/ZLib.cpp -o ZLib.o

ArchiveInfo.o: $(SRCPATH)/ArchiveInfo.cpp $(SRCPATH)/ArchiveInfo.h
	$(CC) $(FLAGS) -c $(SRCPATH)/ArchiveInfo.cpp -o ArchiveInfo.o

Decompress.o: $(SRCPATH)/Decompress.cpp $(SRCPATH)/Decompress.h
	$(CC) $(FLAGS) -c $(SRCPATH)/Decompress.cpp -o Decompress.o

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

