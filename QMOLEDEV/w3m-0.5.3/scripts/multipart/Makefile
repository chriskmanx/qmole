
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

LIB_TARGETS	= multipart.cgi

MKDIR		= mkdir -p
INSTALL		= /usr/bin/install -c
INSTALL_SCRIPT	= ${INSTALL}

PERL		= /usr/local/bin/perl
NKF		= /usr/local/bin/nkf

all: $(LIB_TARGETS)

# do nothing
$(LIB_TARGETS):

install: $(LIB_TARGETS)
	-$(MKDIR) $(DESTDIR)$(CGIBIN_DIR)
	for file in $(LIB_TARGETS);	\
	do	\
		$(INSTALL_SCRIPT) $$file $(DESTDIR)$(CGIBIN_DIR);	\
	done

uninstall:
	-for file in $(LIB_TARGETS);	\
	do	\
		rm -f $(CGIBIN_DIR)/$$file;	\
	done

clean:

distclean:
	-rm -f $(LIB_TARGETS)
	-rm -f Makefile

dist: all
	@-rm -fr $(distdir)/multipart
	$(MKDIR) $(distdir)/multipart
	cp Makefile README multipart.cgi.in $(distdir)/multipart
	( cd $(distdir); tar -cf - multipart | GZIP='' gzip ) \
		> $(distdir)/multipart.tar.gz
	-rm -fr $(distdir)/multipart

