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

#DEBUG=-g -D_DEBUG
#compiler compatible to ZipArchive!
#CXX=g++
CXX=gcc
CFLAGS= -I../ -I./ZipArchive/stl -I./ZipArchive/Linux -I./ZipArchive/
FLAGS= $(CFLAGS)

vpath %.h ./ZipArchive/stl:./ZipArchive/Linux
vpath %.cpp ./ZipArchive/stl:./ZipArchive/Linux

# define compile/link command line with flags
CC=$(CXX) $(DEBUG) -fPIC -O2 -MMD -pthread -Wall -D_REENTRANT
LD=$(CXX) $(DEBUG) -fPIC

#
# define default project to build
#
.PHONY: all

all: ziplib

#
# list of all objects to build (atol dependency)
#

SRCPATH=.

OBJS= ZipLib.o ArchiveInfo.o ./ZipArchive/libziparch.a

# link program binary
# mark symbols with dllexport in source, or try -Wl,--export-all
ziplib: $(OBJS)
	$(LD) $(OBJS) -o ZipLib.atp $(FLAGS) $(LDFLAGS) -shared -fpic --export-all --enable-auto-import

# compile individual objects
# TOFIX?: use pattern rules (how to adjust for possible multiple source directories?)

ZipLib.o: $(SRCPATH)/ZipLib.cpp
	$(CC) $(FLAGS) -c $(SRCPATH)/ZipLib.cpp -o ZipLib.o

ArchiveInfo.o: $(SRCPATH)/ArchiveInfo.cpp $(SRCPATH)/ArchiveInfo.h
	$(CC) $(FLAGS) -c $(SRCPATH)/ArchiveInfo.cpp -o ArchiveInfo.o

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

