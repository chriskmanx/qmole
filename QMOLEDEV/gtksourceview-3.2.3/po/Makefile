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

GETTEXT_PACKAGE = gtksourceview-3.0
PACKAGE = gtksourceview
VERSION = 3.2.3

SHELL = /bin/sh

srcdir = .
top_srcdir = ..
top_builddir = ..


prefix = /usr/local
exec_prefix = ${prefix}
datadir = ${datarootdir}
datarootdir = ${prefix}/share
libdir = ${exec_prefix}/lib
DATADIRNAME = lib
itlocaledir = $(prefix)/$(DATADIRNAME)/locale
subdir = po
install_sh = ${SHELL} /home/chris/gtksourceview-3.2.3/install-sh
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

USER_LINGUAS=$(shell if test -n "$(LINGUAS)"; then LLINGUAS="$(LINGUAS)"; ALINGUAS="$(ALL_LINGUAS)"; for lang in $$LLINGUAS; do if test -n "`grep \^$$lang$$ $(srcdir)/LINGUAS 2>/dev/null`" -o -n "`echo $$ALINGUAS|tr ' ' '\n'|grep \^$$lang$$`"; then printf "$$lang "; fi; done; fi)

USE_LINGUAS=$(shell if test -n "$(USER_LINGUAS)" -o -n "$(LINGUAS)"; then LLINGUAS="$(USER_LINGUAS)"; else if test -n "$(PO_LINGUAS)"; then LLINGUAS="$(PO_LINGUAS)"; else LLINGUAS="$(ALL_LINGUAS)"; fi; fi; for lang in $$LLINGUAS; do printf "$$lang "; done)

POFILES=$(shell LINGUAS="$(PO_LINGUAS)"; for lang in $$LINGUAS; do printf "$$lang.po "; done)

DISTFILES = Makefile.in.in POTFILES.in $(POFILES)
EXTRA_DISTFILES = ChangeLog POTFILES.skip Makevars LINGUAS

POTFILES = \
	../data/glade/gtksourceview.xml.in \
	../data/language-specs/ada.lang \
	../data/language-specs/asp.lang \
	../data/language-specs/awk.lang \
	../data/language-specs/bennugd.lang \
	../data/language-specs/bibtex.lang \
	../data/language-specs/boo.lang \
	../data/language-specs/changelog.lang \
	../data/language-specs/chdr.lang \
	../data/language-specs/c.lang \
	../data/language-specs/cg.lang \
	../data/language-specs/cmake.lang \
	../data/language-specs/cobol.lang \
	../data/language-specs/cpp.lang \
	../data/language-specs/csharp.lang \
	../data/language-specs/css.lang \
	../data/language-specs/cuda.lang \
	../data/language-specs/def.lang \
	../data/language-specs/desktop.lang \
	../data/language-specs/diff.lang \
	../data/language-specs/d.lang \
	../data/language-specs/docbook.lang \
	../data/language-specs/dosbatch.lang \
	../data/language-specs/dot.lang \
	../data/language-specs/dpatch.lang \
	../data/language-specs/dtd.lang \
	../data/language-specs/eiffel.lang \
	../data/language-specs/erlang.lang \
	../data/language-specs/fcl.lang \
	../data/language-specs/forth.lang \
	../data/language-specs/fortran.lang \
	../data/language-specs/fsharp.lang \
	../data/language-specs/gap.lang \
	../data/language-specs/gdb-log.lang \
	../data/language-specs/glsl.lang \
	../data/language-specs/go.lang \
	../data/language-specs/gtk-doc.lang \
	../data/language-specs/gtkrc.lang \
	../data/language-specs/haddock.lang \
	../data/language-specs/haskell.lang \
	../data/language-specs/haskell-literate.lang \
	../data/language-specs/html.lang \
	../data/language-specs/idl.lang \
	../data/language-specs/imagej.lang \
	../data/language-specs/ini.lang \
	../data/language-specs/java.lang \
	../data/language-specs/javascript.lang \
	../data/language-specs/json.lang \
	../data/language-specs/latex.lang \
	../data/language-specs/libtool.lang \
	../data/language-specs/lua.lang \
	../data/language-specs/m4.lang \
	../data/language-specs/makefile.lang \
	../data/language-specs/mallard.lang \
	../data/language-specs/markdown.lang \
	../data/language-specs/matlab.lang \
	../data/language-specs/nemerle.lang \
	../data/language-specs/nsis.lang \
	../data/language-specs/objc.lang \
	../data/language-specs/objj.lang \
	../data/language-specs/ocaml.lang \
	../data/language-specs/ocl.lang \
	../data/language-specs/octave.lang \
	../data/language-specs/ooc.lang \
	../data/language-specs/opal.lang \
	../data/language-specs/opencl.lang \
	../data/language-specs/pascal.lang \
	../data/language-specs/perl.lang \
	../data/language-specs/php.lang \
	../data/language-specs/pkgconfig.lang \
	../data/language-specs/po.lang \
	../data/language-specs/prolog.lang \
	../data/language-specs/protobuf.lang \
	../data/language-specs/python.lang \
	../data/language-specs/R.lang \
	../data/language-specs/rpmspec.lang \
	../data/language-specs/ruby.lang \
	../data/language-specs/scheme.lang \
	../data/language-specs/scilab.lang \
	../data/language-specs/sh.lang \
	../data/language-specs/sml.lang \
	../data/language-specs/sparql.lang \
	../data/language-specs/sql.lang \
	../data/language-specs/systemverilog.lang \
	../data/language-specs/t2t.lang \
	../data/language-specs/tcl.lang \
	../data/language-specs/texinfo.lang \
	../data/language-specs/vala.lang \
	../data/language-specs/vbnet.lang \
	../data/language-specs/verilog.lang \
	../data/language-specs/vhdl.lang \
	../data/language-specs/xml.lang \
	../data/language-specs/xslt.lang \
	../data/language-specs/yacc.lang \
	../data/styles/classic.xml \
	../data/styles/cobalt.xml \
	../data/styles/kate.xml \
	../data/styles/oblivion.xml \
	../data/styles/tango.xml \
	../gtksourceview/completion-providers/words/gtksourcecompletionwords.c \
	../gtksourceview/gtksourcebuffer.c \
	../gtksourceview/gtksourcecompletion.c \
	../gtksourceview/gtksourcecompletioncontext.c \
	../gtksourceview/gtksourcecompletioninfo.c \
	../gtksourceview/gtksourcecompletionitem.c \
	../gtksourceview/gtksourcecontextengine.c \
	../gtksourceview/gtksourcegutter.c \
	../gtksourceview/gtksourcegutterrenderer.c \
	../gtksourceview/gtksourcegutterrendererpixbuf.c \
	../gtksourceview/gtksourcegutterrenderertext.c \
	../gtksourceview/gtksourcelanguage.c \
	../gtksourceview/gtksourcelanguagemanager.c \
	../gtksourceview/gtksourcelanguage-parser-2.c \
	../gtksourceview/gtksourcemark.c \
	../gtksourceview/gtksourcemarkattributes.c \
	../gtksourceview/gtksourceprintcompositor.c \
	../gtksourceview/gtksourcestyle.c \
	../gtksourceview/gtksourcestylescheme.c \
	../gtksourceview/gtksourcestyleschememanager.c \
	../gtksourceview/gtksourceundomanagerdefault.c \
	../gtksourceview/gtksourceview.c \
	../gtksourceview/gtksourceview-i18n.c

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
dvi info ctags tags CTAGS TAGS ID:

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
