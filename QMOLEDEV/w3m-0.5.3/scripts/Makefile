
SHELL=/bin/sh
PACKAGE = w3m
VERSION = 0.5.3
DOMAIN = $(PACKAGE)
srcidr=.
top_srcdir=..
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
top_srcdir = ..
VPATH = .

CGIBIN_DIR = $(libexecdir)/$(PACKAGE)/cgi-bin
AUXBIN_DIR = $(libexecdir)/$(PACKAGE)
HELP_DIR = ${datarootdir}/w3m
RC_DIR = ~/.w3m
ETC_DIR = $(sysconfdir)
CONF_DIR = $(sysconfdir)/$(PACKAGE)
DOCDIRS = doc:en_English doc-jp:ja_Japanese

AUXBIN_TARGETS	= xface2xpm
LIB_TARGETS	= dirlist.cgi w3mhelp.cgi w3mmail.cgi
HELP_TARGETS	= w3mhelp-funcname.pl w3mhelp-funcdesc-stamp
SUBDIRS = multipart w3mman
.PHONY: $(SUBDIRS)

MKDIR		= mkdir -p
INSTALL		= /usr/bin/install -c
INSTALL_SCRIPT	= ${INSTALL}
INSTALL_DATA	= ${INSTALL} -m 644

PERL		= /usr/local/bin/perl

all: $(LIB_TARGETS) $(AUXBIN_TARGETS) $(HELP_TARGETS) $(SUBDIRS)

w3mhelp-funcname.pl: w3mhelp-funcname.pl.in ../funcname.tab $(top_srcdir)/doc/keymap.default $(top_srcdir)/doc/keymap.lynx
	@echo "generating w3mhelp-funcname.pl..."
	@echo '%funcname = (' > w3mhelp-funcname.pl
	@sed -ne "/^[a-zA-Z@]/s/\([a-zA-Z@][a-zA-Z0-9@_]*\)[ 	][ 	]*\([^ 	]*\)/'\1', '\2',/p" < ../funcname.tab >> w3mhelp-funcname.pl
	@echo ');' >> w3mhelp-funcname.pl
	@echo '%keyfunc = (' >> w3mhelp-funcname.pl
	@case "$(KEYBIND_SRC)" in *lynx*) keymap=keymap.lynx;; *) keymap=keymap.default;; esac; \
	sed -ne "/^keymap/s/^keymap[ 	][ 	]*\([^ 	]*\)[ 	][ 	]*\([a-zA-Z@][a-zA-Z0-9@_]*\)/'\1', '\2', /p" < $(top_srcdir)/doc/$$keymap >> w3mhelp-funcname.pl
	@echo ');' >> w3mhelp-funcname.pl
	@cat $(srcdir)/w3mhelp-funcname.pl.in >> w3mhelp-funcname.pl
	@echo "done"

w3mhelp-funcdesc-stamp: $(top_srcdir)/doc/README.func $(top_srcdir)/doc-jp/README.func w3mhelp-funcdesc.en.pl.in w3mhelp-funcdesc.ja.pl.in
	@echo "generating w3mhelp-funcdesc*.pl..."
	@for dirlang in $(DOCDIRS); do \
	  dir=`expr "$$dirlang" : "\(.*\):.*"`; \
	  lang=`expr "$$dirlang" : ".*:\(.*\)_.*"`; \
	  echo '%funcdesc = (' > w3mhelp-funcdesc.$$lang.pl; \
	  sed -ne "/^[a-zA-Z@]/s/\([a-zA-Z@][a-zA-Z0-9@_]*\)[ 	][ 	]*\(.*\)/'\1', '\2',/p" < $(top_srcdir)/$$dir/README.func >> w3mhelp-funcdesc.$$lang.pl; \
	  echo ');' >> w3mhelp-funcdesc.$$lang.pl; \
	  cat $(srcdir)/w3mhelp-funcdesc.$$lang.pl.in >> w3mhelp-funcdesc.$$lang.pl; \
	done
	@echo done
	@touch w3mhelp-funcdesc-stamp

$(SUBDIRS):
	for subdir in $(SUBDIRS); \
	do \
		(cd $$subdir && $(MAKE)); \
	done

install: $(LIB_TARGETS) $(HELP_TARGETS)
	-$(MKDIR) $(DESTDIR)$(AUXBIN_DIR)
	-$(MKDIR) $(DESTDIR)$(CGIBIN_DIR)
	-$(MKDIR) $(DESTDIR)$(HELP_DIR)
	for file in $(AUXBIN_TARGETS);     \
	do      \
		$(INSTALL_SCRIPT) $$file $(DESTDIR)$(AUXBIN_DIR);  \
	done
	for file in $(LIB_TARGETS);     \
	do      \
		$(INSTALL_SCRIPT) $$file $(DESTDIR)$(CGIBIN_DIR);  \
	done
	for file in w3mhelp-*.pl; \
	do \
		$(INSTALL_DATA) $$file $(DESTDIR)$(HELP_DIR); \
	done
	for subdir in $(SUBDIRS); \
	do \
		(cd $$subdir && $(MAKE) install); \
	done

uninstall:
	-for file in $(AUXBIN_TARGETS); \
	do      \
		rm -f $(AUXBIN_DIR)/$$file; \
	done
	-for file in $(LIB_TARGETS); \
	do      \
		rm -f $(LIB_DIR)/$$file; \
	done
	-for file in w3mhelp-*.pl; \
	do \
		rm -f $(HELP_DIR)/$$file; \
	done
	-for subdir in $(SUBDIRS); \
	do \
		(cd $$subdir && $(MAKE) uninstall); \
	done

clean:
	-rm -f $(HELP_TARGETS) w3mhelp-*.pl
	-for subdir in $(SUBDIRS); \
	do \
	  (cd $$subdir && $(MAKE) clean); \
	done

distclean:
	-rm -f $(LIB_TARGETS) $(AUXBIN_TARGETS) 
	-for subdir in $(SUBDIRS); \
	do \
	  (cd $$subdir && $(MAKE) distclean); \
	done
	-rm -f Makefile

