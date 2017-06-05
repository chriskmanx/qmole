#
# w3mimg/win/Makefile
#
#

SHELL=/bin/sh
srcdir = .
top_srcdir = ../..
VPATH = .
CFLAGS=$(OPTS) -I../.. -I$(top_srcdir) -I$(srcdir) -g -O2 -I$(srcdir)/libwc   -I/usr/local/include -DHAVE_CONFIG_H $(IMGCFLAGS)
RM=rm
CC=/usr/bin/gcc
CXX=g++

IMGCFLAGS=
OBJS=win_w3mimg.o

all: win_w3mimg.o

win_w3mimg.o: win_w3mimg.cpp
	$(CXX) $(CFLAGS) -c $<

clean:
	@-$(RM) -f *.o

distclean: clean
	-$(RM) -f Makefile

#


