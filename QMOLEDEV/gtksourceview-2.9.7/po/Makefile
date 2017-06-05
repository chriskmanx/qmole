# Makefile for program source directory in GNU NLS utilities package.
# Copyright (C) 1995, 1996, 1997 by Ulrich Drepper <drepper@gnu.ai.mit.edu>
# Copyright (C) 2004-2008 Rodney Dawes <dobey.pwns@gmail.com>
#
# This file may be copied and used freely without restrictions.  It may
# be used in projects which are not available under a GNU Public License,
# but which still want to provide support for the GNU gettext functionality.
#
# - Modified by Owen Taylor <otaylor@redhat.com> to use GETTEXT_PACKAGE
#   instead of PACKAGE and to look for po2tbl in ./ not in intl/
#
# - Modified by jacob berkman <jacob@ximian.com> to install
#   Makefile.in.in and po2tbl.sed.in for use with glib-gettextize
#
# - Modified by Rodney Dawes <dobey.pwns@gmail.com> for use with intltool
#
# We have the following line for use by intltoolize:
# INTLTOOL_MAKEFILE

GETTEXT_PACKAGE = gtksourceview-2.0
PACKAGE = gtksourceview
VERSION = 2.9.7

SHELL = /bin/sh

srcdir = .
top_srcdir = ..
top_builddir = ..


prefix = /usr/local
exec_prefix = ${prefix}
datadir = ${datarootdir}
datarootdir = ${prefix}/share
libdir = ${exec_prefix}/lib
DATADIRNAME = share
itlocaledir = $(prefix)/$(DATADIRNAME)/locale
subdir = po
install_sh = ${SHELL} /home/chris/gtksourceview-2.9.7/install-sh
# Automake >= 1.8 provides /bin/mkdir -p.
# Until it can be supposed, use the safe fallback:
mkdir_p = $(install_sh) -d

INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644

GMSGFMT = /usr/local/bin/msgfmt
MSGFMT = /usr/local/bin/msgfmt
XGETTEXT = /usr/local/bin/xgettext
INTLTOOL_UPDATE = /usr/local/bin/intltool-update
INTLTOOL_EXTRACT = /usr/local/bin/intltool-extract
MSGMERGE = INTLTOOL_EXTRACT=$(INTLTOOL_EXTRACT) srcdir=$(srcdir) $(INTLTOOL_UPDATE) --gettext-package $(GETTEXT_PACKAGE) --dist
GENPOT   = INTLTOOL_EXTRACT=$(INTLTOOL_EXTRACT) srcdir=$(srcdir) $(INTLTOOL_UPDATE) --gettext-package $(GETTEXT_PACKAGE) --pot

ALL_LINGUAS = 

PO_LINGUAS=$(shell if test -r $(srcdir)/LINGUAS; then grep -v "^\#" $(srcdir)/LINGUAS; else echo "$(ALL_LINGUAS)"; fi)

USER_LINGUAS=$(shell if test -n "$(LINGUAS)"; then LLINGUAS="$(LINGUAS)"; ALINGUAS="$(ALL_LINGUAS)"; for lang in $$LLINGUAS; do if test -n "`grep '^$$lang$$' $(srcdir)/LINGUAS 2>/dev/null`" -o -n "`echo $$ALINGUAS|tr ' ' '\n'|grep '^$$lang$$'`"; then printf "$$lang "; fi; done; fi)

USE_LINGUAS=$(shell if test -n "$(USER_LINGUAS)" -o -n "$(LINGUAS)"; then LLINGUAS="$(USER_LINGUAS)"; else if test -n "$(PO_LINGUAS)"; then LLINGUAS="$(PO_LINGUAS)"; else LLINGUAS="$(ALL_LINGUAS)"; fi; fi; for lang in $$LLINGUAS; do printf "$$lang "; done)

POFILES=$(shell LINGUAS="$(PO_LINGUAS)"; for lang in $$LINGUAS; do printf "$$lang.po "; done)

DISTFILES = Makefile.in.in POTFILES.in $(POFILES)
EXTRA_DISTFILES = ChangeLog POTFILES.skip Makevars LINGUAS

POTFILES = \
	../gtksourceview/completion-providers/words/gtksourcecompletionwords.c \
	../gtksourceview/gtksourcebuffer.c \
	../gtksourceview/gtksourcecompletion.c \
	../gtksourceview/gtksourcecompletioncontext.c \
	../gtksourceview/gtksourcecompletioninfo.c \
	../gtksourceview/gtksourcecompletionitem.c \
	../gtksourceview/gtksourcecontextengine.c \
	../gtksourceview/gtksourcegutter.c \
	../gtksourceview/gtksourcelanguage.c \
	../gtksourceview/gtksourcelanguagemanager.c \
	../gtksourceview/gtksourcelanguage-parser-2.c \
	../gtksourceview/gtksourcemark.c \
	../gtksourceview/gtksourceprintcompositor.c \
	../gtksourceview/gtksourcestyle.c \
	../gtksourceview/gtksourcestylescheme.c \
	../gtksourceview/gtksourcestyleschememanager.c \
	../gtksourceview/gtksourceundomanagerdefault.c \
	../gtksourceview/gtksourceview.c \
	../gtksourceview/gtksourceview-i18n.c \
	../gtksourceview/language-specs/ada.lang \
	../gtksourceview/language-specs/asp.lang \
	../gtksourceview/language-specs/awk.lang \
	../gtksourceview/language-specs/bibtex.lang \
	../gtksourceview/language-specs/boo.lang \
	../gtksourceview/language-specs/changelog.lang \
	../gtksourceview/language-specs/chdr.lang \
	../gtksourceview/language-specs/c.lang \
	../gtksourceview/language-specs/cg.lang \
	../gtksourceview/language-specs/classic.xml \
	../gtksourceview/language-specs/cmake.lang \
	../gtksourceview/language-specs/cobalt.xml \
	../gtksourceview/language-specs/cpp.lang \
	../gtksourceview/language-specs/csharp.lang \
	../gtksourceview/language-specs/css.lang \
	../gtksourceview/language-specs/cuda.lang \
	../gtksourceview/language-specs/def.lang \
	../gtksourceview/language-specs/desktop.lang \
	../gtksourceview/language-specs/diff.lang \
	../gtksourceview/language-specs/d.lang \
	../gtksourceview/language-specs/docbook.lang \
	../gtksourceview/language-specs/dosbatch.lang \
	../gtksourceview/language-specs/dot.lang \
	../gtksourceview/language-specs/dpatch.lang \
	../gtksourceview/language-specs/dtd.lang \
	../gtksourceview/language-specs/eiffel.lang \
	../gtksourceview/language-specs/erlang.lang \
	../gtksourceview/language-specs/fcl.lang \
	../gtksourceview/language-specs/forth.lang \
	../gtksourceview/language-specs/fortran.lang \
	../gtksourceview/language-specs/fsharp.lang \
	../gtksourceview/language-specs/gap.lang \
	../gtksourceview/language-specs/glsl.lang \
	../gtksourceview/language-specs/gtk-doc.lang \
	../gtksourceview/language-specs/gtkrc.lang \
	../gtksourceview/language-specs/haddock.lang \
	../gtksourceview/language-specs/haskell.lang \
	../gtksourceview/language-specs/haskell-literate.lang \
	../gtksourceview/language-specs/html.lang \
	../gtksourceview/language-specs/idl.lang \
	../gtksourceview/language-specs/ini.lang \
	../gtksourceview/language-specs/java.lang \
	../gtksourceview/language-specs/javascript.lang \
	../gtksourceview/language-specs/kate.xml \
	../gtksourceview/language-specs/latex.lang \
	../gtksourceview/language-specs/libtool.lang \
	../gtksourceview/language-specs/lua.lang \
	../gtksourceview/language-specs/m4.lang \
	../gtksourceview/language-specs/makefile.lang \
	../gtksourceview/language-specs/mallard.lang \
	../gtksourceview/language-specs/msil.lang \
	../gtksourceview/language-specs/msil.lang \
	../gtksourceview/language-specs/nemerle.lang \
	../gtksourceview/language-specs/nsis.lang \
	../gtksourceview/language-specs/objc.lang \
	../gtksourceview/language-specs/oblivion.xml \
	../gtksourceview/language-specs/ocaml.lang \
	../gtksourceview/language-specs/ocl.lang \
	../gtksourceview/language-specs/octave.lang \
	../gtksourceview/language-specs/ooc.lang \
	../gtksourceview/language-specs/pascal.lang \
	../gtksourceview/language-specs/perl.lang \
	../gtksourceview/language-specs/php.lang \
	../gtksourceview/language-specs/pkgconfig.lang \
	../gtksourceview/language-specs/po.lang \
	../gtksourceview/language-specs/prolog.lang \
	../gtksourceview/language-specs/python.lang \
	../gtksourceview/language-specs/R.lang \
	../gtksourceview/language-specs/rpmspec.lang \
	../gtksourceview/language-specs/ruby.lang \
	../gtksourceview/language-specs/scheme.lang \
	../gtksourceview/language-specs/sh.lang \
	../gtksourceview/language-specs/sparql.lang \
	../gtksourceview/language-specs/sql.lang \
	../gtksourceview/language-specs/t2t.lang \
	../gtksourceview/language-specs/tango.xml \
	../gtksourceview/language-specs/tcl.lang \
	../gtksourceview/language-specs/texinfo.lang \
	../gtksourceview/language-specs/vala.lang \
	../gtksourceview/language-specs/vbnet.lang \
	../gtksourceview/language-specs/verilog.lang \
	../gtksourceview/language-specs/vhdl.lang \
	../gtksourceview/language-specs/xml.lang \
	../gtksourceview/language-specs/xslt.lang \
	../gtksourceview/language-specs/yacc.lang

CATALOGS=$(shell LINGUAS="$(USE_LINGUAS)"; for lang in $$LINGUAS; do printf "$$lang.gmo "; done)

.SUFFIXES:
.SUFFIXES: .po .pox .gmo .mo .msg .cat

.po.pox:
	$(MAKE) $(GETTEXT_PACKAGE).pot
	$(MSGMERGE) $< $(GETTEXT_PACKAGE).pot -o $*.pox

.po.mo:
	$(MSGFMT) -o $@ $<

.po.gmo:
	file=`echo $* | sed 's,.*/,,'`.gmo \
	  && rm -f $$file && $(GMSGFMT) -o $$file $<

.po.cat:
	sed -f ../intl/po2msg.sed < $< > $*.msg \
	  && rm -f $@ && gencat $@ $*.msg


all: all-yes

all-yes: $(CATALOGS)
all-no:

$(GETTEXT_PACKAGE).pot: $(POTFILES)
	$(GENPOT)

install: install-data
install-data: install-data-yes
install-data-no: all
install-data-yes: all
	linguas="$(USE_LINGUAS)"; \
	for lang in $$linguas; do \
	  dir=$(DESTDIR)$(itlocaledir)/$$lang/LC_MESSAGES; \
	  $(mkdir_p) $$dir; \
	  if test -r $$lang.gmo; then \
	    $(INSTALL_DATA) $$lang.gmo $$dir/$(GETTEXT_PACKAGE).mo; \
	    echo "installing $$lang.gmo as $$dir/$(GETTEXT_PACKAGE).mo"; \
	  else \
	    $(INSTALL_DATA) $(srcdir)/$$lang.gmo $$dir/$(GETTEXT_PACKAGE).mo; \
	    echo "installing $(srcdir)/$$lang.gmo as" \
		 "$$dir/$(GETTEXT_PACKAGE).mo"; \
	  fi; \
	  if test -r $$lang.gmo.m; then \
	    $(INSTALL_DATA) $$lang.gmo.m $$dir/$(GETTEXT_PACKAGE).mo.m; \
	    echo "installing $$lang.gmo.m as $$dir/$(GETTEXT_PACKAGE).mo.m"; \
	  else \
	    if test -r $(srcdir)/$$lang.gmo.m ; then \
	      $(INSTALL_DATA) $(srcdir)/$$lang.gmo.m \
		$$dir/$(GETTEXT_PACKAGE).mo.m; \
	      echo "installing $(srcdir)/$$lang.gmo.m as" \
		   "$$dir/$(GETTEXT_PACKAGE).mo.m"; \
	    else \
	      true; \
	    fi; \
	  fi; \
	done

# Empty stubs to satisfy archaic automake needs
dvi info tags TAGS ID:

# Define this as empty until I found a useful application.
install-exec installcheck:

uninstall:
	linguas="$(USE_LINGUAS)"; \
	for lang in $$linguas; do \
	  rm -f $(DESTDIR)$(itlocaledir)/$$lang/LC_MESSAGES/$(GETTEXT_PACKAGE).mo; \
	  rm -f $(DESTDIR)$(itlocaledir)/$$lang/LC_MESSAGES/$(GETTEXT_PACKAGE).mo.m; \
	done

check: all $(GETTEXT_PACKAGE).pot
	rm -f missing notexist
	srcdir=$(srcdir) $(INTLTOOL_UPDATE) -m
	if [ -r missing -o -r notexist ]; then \
	  exit 1; \
	fi

mostlyclean:
	rm -f *.pox $(GETTEXT_PACKAGE).pot *.old.po cat-id-tbl.tmp
	rm -f .intltool-merge-cache

clean: mostlyclean

distclean: clean
	rm -f Makefile Makefile.in POTFILES stamp-it
	rm -f *.mo *.msg *.cat *.cat.m *.gmo

maintainer-clean: distclean
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."
	rm -f Makefile.in.in

distdir = ../$(PACKAGE)-$(VERSION)/$(subdir)
dist distdir: $(DISTFILES)
	dists="$(DISTFILES)"; \
	extra_dists="$(EXTRA_DISTFILES)"; \
	for file in $$extra_dists; do \
	  test -f $(srcdir)/$$file && dists="$$dists $(srcdir)/$$file"; \
	done; \
	for file in $$dists; do \
	  test -f $$file || file="$(srcdir)/$$file"; \
	  ln $$file $(distdir) 2> /dev/null \
	    || cp -p $$file $(distdir); \
	done

update-po: Makefile
	$(MAKE) $(GETTEXT_PACKAGE).pot
	tmpdir=`pwd`; \
	linguas="$(USE_LINGUAS)"; \
	for lang in $$linguas; do \
	  echo "$$lang:"; \
	  result="`$(MSGMERGE) -o $$tmpdir/$$lang.new.po $$lang`"; \
	  if $$result; then \
	    if cmp $(srcdir)/$$lang.po $$tmpdir/$$lang.new.po >/dev/null 2>&1; then \
	      rm -f $$tmpdir/$$lang.new.po; \
            else \
	      if mv -f $$tmpdir/$$lang.new.po $$lang.po; then \
	        :; \
	      else \
	        echo "msgmerge for $$lang.po failed: cannot move $$tmpdir/$$lang.new.po to $$lang.po" 1>&2; \
	        rm -f $$tmpdir/$$lang.new.po; \
	        exit 1; \
	      fi; \
	    fi; \
	  else \
	    echo "msgmerge for $$lang.gmo failed!"; \
	    rm -f $$tmpdir/$$lang.new.po; \
	  fi; \
	done

Makefile POTFILES: stamp-it
	@if test ! -f $@; then \
	  rm -f stamp-it; \
	  $(MAKE) stamp-it; \
	fi

stamp-it: Makefile.in.in $(top_builddir)/config.status POTFILES.in
	cd $(top_builddir) \
	  && CONFIG_FILES=$(subdir)/Makefile.in CONFIG_HEADERS= CONFIG_LINKS= \
	       $(SHELL) ./config.status

# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
