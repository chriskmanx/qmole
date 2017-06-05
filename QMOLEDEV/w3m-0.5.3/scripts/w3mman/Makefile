
SHELL=/bin/sh
PACKAGE = w3m
VERSION = 0.5.3
DOMAIN = $(PACKAGE)
srcidr=.
top_srcdir=../..
VPATH= .
prefix = /usr/local
exec_prefix = ${prefix}
datarootdir = ${prefix}/share
bindir = ${exec_prefix}/bin
datadir = ${datarootdir}
libdir = ${exec_prefix}/lib
includedir = ${prefix}/include
infodir = ${datarootdir}/info
libexecdir = ${exec_prefix}/libexec
localstatedir = ${prefix}/var
mandir = ${datarootdir}/man
oldincludedir = /usr/include
sbindir = ${exec_prefix}/sbin
sharedstatedir = ${prefix}/com
srcdir = .
sysconfdir = ${prefix}/etc
top_srcdir = ../..
VPATH = $(top_srcdir):.

CGIBIN_DIR = $(libexecdir)/$(PACKAGE)/cgi-bin
AUXBIN_DIR = $(libexecdir)/$(PACKAGE)
HELP_DIR = ${datarootdir}/w3m
RC_DIR = ~/.w3m
ETC_DIR = $(sysconfdir)
CONF_DIR = $(sysconfdir)/$(PACKAGE)
BIN_DIR = $(bindir)
MAN1_DIR = $(mandir)/man1

TARGETS		= w3mman
CGIBIN_TARGETS	= w3mman2html.cgi
MAN1_TARGETS	= w3mman.1
MAN_TARGETS	= $(MAN1_TARGETS)

MKDIR		= mkdir -p
INSTALL		= /usr/bin/install -c
INSTALL_SCRIPT	= ${INSTALL}
INSTALL_MAN	= ${INSTALL} -m 644

all: $(TARGETS) $(CGIBIN_TARGETS) $(MAN_TARGETS)

# do nothing
$(TARGETS) $(CGIBIN_TARGETS) $(MAN_TARGETS):

install: $(TARGETS) $(CGIBIN_TARGETS) $(MAN_TARGETS)
	-$(MKDIR) $(DESTDIR)$(BIN_DIR)
	-$(MKDIR) $(DESTDIR)$(CGIBIN_DIR)
	-$(MKDIR) $(DESTDIR)$(MAN1_DIR)
	for file in $(TARGETS);     \
	do      \
		$(INSTALL_SCRIPT) $$file $(DESTDIR)$(BIN_DIR);	\
	done
	for file in $(CGIBIN_TARGETS);     \
	do      \
		$(INSTALL_SCRIPT) $$file $(DESTDIR)$(CGIBIN_DIR);	\
	done
	for file in $(MAN1_TARGETS); \
	do	\
		$(INSTALL_MAN) $$file $(DESTDIR)$(MAN1_DIR); \
	done

uninstall:
	-for file in $(TARGETS);     \
	do      \
		rm -f $(BIN_DIR)/$$file;	\
	done
	-for file in $(CGIBIN_TARGETS);     \
	do      \
		rm -f $(CGIBIN_DIR)/$$file;	\
	done
	-for file in $(MAN1_TARGETS); \
	do \
		rm -f $(MAN1_DIR)/$$file; \
	done

clean:

distclean:
	-rm -f $(TARGETS) $(CGIBIN_TARGETS) $(MAN_TARGETS)
	-rm -f Makefile

dist: all
	@-rm -fr $(distdir)/w3mman
	-$(MKDIR) $(distdir)/w3mman
	cp Makefile README w3mman.in w3mman2html.cgi.in hlink.cgi w3mman.1.in $(distdir)/w3mman
	( cd $(distdir); tar -cf - w3mman | GZIP='' gzip ) \
		> $(distdir)/w3mman.tar.gz
	-rm -fr $(distdir)/w3mman

