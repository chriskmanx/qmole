# Generated automatically from Makefile.in:boilerplate.mk by configure.

SRCS = Exception.cxx FdInStream.cxx FdOutStream.cxx InStream.cxx \
       RandomStream.cxx ZlibInStream.cxx ZlibOutStream.cxx \
       HexInStream.cxx HexOutStream.cxx

OBJS = $(SRCS:.cxx=.o)

DIR_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/zlib

library = librdr.a

all:: $(library)

$(library): $(OBJS)
	rm -f $(library)
	$(AR) $(library) $(OBJS)
	$(RANLIB) $(library)

# followed by boilerplate.mk

all::
	@subdirs="$(SUBDIRS)"; for d in $$subdirs; do (cd $$d; $(MAKE) $@) || exit 1; done

clean::
	@subdirs="$(SUBDIRS)"; for d in $$subdirs; do (cd $$d; $(MAKE) $@) || exit 1; done

clean::
	rm -f $(program) $(library) *.o

SHELL = /bin/sh
top_srcdir = ..

CC = /usr/bin/gcc
CFLAGS = -O2 -Wall $(DIR_CFLAGS)
CCLD = $(CC)
CXX = c++
CXXFLAGS = -O2 -Wall
CXXLD = $(CXX)
CPPFLAGS = 
DEFS =  -DX_DISPLAY_MISSING=1 
ALL_CPPFLAGS = $(CPPFLAGS) $(DEFS) $(DIR_CPPFLAGS)
LIBS = 
LDFLAGS = 
RANLIB = ranlib
AR = ar cq

.SUFFIXES:
.SUFFIXES: .cxx .c .o

.c.o:
	$(CC) $(ALL_CPPFLAGS) $(CFLAGS) -c $<

.cxx.o:
	$(CXX) $(ALL_CPPFLAGS) $(CXXFLAGS) -c $<
