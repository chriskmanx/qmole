
prefix      = /usr/local
exec_prefix = ${prefix}
bindir      = ${exec_prefix}/bin
mandir      = ${datarootdir}/man
datarootdir = ${prefix}/share
datadir     = ${datarootdir}

VERSION     = 0.12

CC          = /usr/bin/gcc
INSTALL     = /usr/bin/install -c
CFLAGS      = -g -O2 -Wall  -DPACKAGE_NAME=\"xclip\" -DPACKAGE_TARNAME=\"xclip\" -DPACKAGE_VERSION=\"0.12\" -DPACKAGE_STRING=\"xclip\ 0.12\" -DPACKAGE_BUGREPORT=\"\" -DHAVE_SYS_TYPES_H=1 -DHAVE_SYS_STAT_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_MEMORY_H=1 -DHAVE_STRINGS_H=1 -DHAVE_INTTYPES_H=1 -DHAVE_STDINT_H=1 -DHAVE_UNISTD_H=1 -DHAVE_LIBXMU=1
LDFLAGS     =  -lXmu   
STRIP       = strip

OBJS = xclib.o xcprint.o xclip.o

.PHONY: all
all: xclip

xclip: $(X11OBJ) $(OBJS)
	$(CC) $(OBJS) $(CFLAGS) -o $@ $(X11OBJ) $(LDFLAGS) -lX11 -lXmu

install: installbin install.man

.PHONY: installbin
installbin: xclip xclip-copyfile xclip-pastefile xclip-cutfile
	mkdir -p $(DESTDIR)$(bindir)
	$(INSTALL) $^ $(DESTDIR)$(bindir)


.PHONY: install.man
install.man: xclip.1 xclip-copyfile.1
	mkdir -p $(DESTDIR)$(mandir)/man1
	$(INSTALL) -m 644 $^ $(DESTDIR)$(mandir)/man1

.PHONY: clean
clean:
	rm -f *.o *~ xclip xclip-$(VERSION).tar.gz

.PHONY: distclean
distclean: clean
	rm -rf autom4te.cache config.log config.status Makefile

.PHONY: dist
dist: xclip-$(VERSION).tar.gz

.PHONY: xclip-$(VERSION).tar.gz
xclip-$(VERSION).tar.gz: 
	mkdir -p /tmp/xclip-make-dist-dir
	ln -sf `pwd` /tmp/xclip-make-dist-dir/xclip-$(VERSION)
	(cd /tmp/xclip-make-dist-dir; \
	tar zcvf xclip-$(VERSION)/xclip-$(VERSION).tar.gz \
	xclip-$(VERSION)/COPYING \
	xclip-$(VERSION)/README \
	xclip-$(VERSION)/ChangeLog \
	xclip-$(VERSION)/configure \
	xclip-$(VERSION)/configure.ac \
	xclip-$(VERSION)/bootstrap \
	xclip-$(VERSION)/xclip-copyfile \
	xclip-$(VERSION)/xclip-pastefile \
	xclip-$(VERSION)/xclip-cutfile \
	xclip-$(VERSION)/install-sh \
	xclip-$(VERSION)/Makefile.in \
	xclip-$(VERSION)/xclip.spec \
	xclip-$(VERSION)/*.c \
	xclip-$(VERSION)/*.h \
	xclip-$(VERSION)/xclip-copyfile.1 \
	xclip-$(VERSION)/xclip.1 )
	rm -rf /tmp/xclip-make-dist-dir

Makefile: Makefile.in configure
	./config.status

configure: configure.ac
	./bootstrap

.SUFFIXES:
.SUFFIXES: .c .o

.c.o:
	$(CC) $(CFLAGS) -o $@ -c $<

