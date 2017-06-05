#
# w3mimg/x11/Makefile
#
#

SHELL=/bin/sh
srcdir = .
top_srcdir = ../..
VPATH = .
CFLAGS=$(OPTS) -I../.. -I$(top_srcdir) -I$(srcdir) -g -O2 -I$(srcdir)/libwc   -I/usr/local/include -DHAVE_CONFIG_H $(IMGCFLAGS)
RM=rm

IMGCFLAGS=-D_REENTRANT -I/usr/local/include/gdk-pixbuf-2.0 -I/usr/local/include/libpng14 -I/usr/local/lib/glib-2.0/include -I/usr/local/include/gtk-2.0 -I/usr/local/lib/gtk-2.0/include -I/usr/local/include/atk-1.0 -I/usr/local/include/cairo -I/usr/local/include/pango-1.0 -I/usr/local/include/pixman-1 -I/usr/local/include/freetype2  
OBJS=x11_w3mimg.o

all: x11_w3mimg.o

x11_w3mimg.o: x11_w3mimg.c
	$(CC) $(CFLAGS) -c $<

clean:
	@-$(RM) -f *.o

distclean: clean
	-$(RM) -f Makefile

#


