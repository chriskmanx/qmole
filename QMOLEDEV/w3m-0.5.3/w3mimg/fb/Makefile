#
# w3mimg/fb/Makefile
#
#

SHELL=/bin/sh
srcdir = .
top_srcdir = ../..
VPATH = .
CFLAGS=$(OPTS) -I../.. -I$(top_srcdir) -I$(srcdir) -g -O2 -I$(srcdir)/libwc   -I/usr/local/include -DHAVE_CONFIG_H $(IMGCFLAGS)
RM=rm

IMGCFLAGS=
OBJS=fb.o fb_img.o 

all: fb_w3mimg.o fb.o fb_img.o

fb_w3mimg.o: fb_w3mimg.c
	$(CC) $(CFLAGS) -c $<

fb.o: fb.c
	$(CC) $(CFLAGS) -c $<

fb_img.o: fb_img.c fb_gdkpixbuf.c fb_imlib2.c
	$(CC) $(CFLAGS) -c $<

clean:
	@-$(RM) -f *.o

distclean: clean
	-$(RM) -f Makefile

#


