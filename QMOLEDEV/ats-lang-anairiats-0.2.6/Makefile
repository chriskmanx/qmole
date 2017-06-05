#########################################################################
##                                                                     ##
##                         Applied Type System                         ##
##                                                                     ##
##                              Hongwei Xi                             ##
##                                                                     ##
#########################################################################

##
## ATS/Anairiats - Unleashing the Potential of Types!
##
## Copyright (C) 2002-2008 Hongwei Xi.
##
## ATS is  free software;  you can redistribute it and/or modify it under
## the  terms of the  GNU General Public License as published by the Free
## Software Foundation; either version 2.1, or (at your option) any later
## version.
##
## ATS is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
## for more details.
##
## You  should  have  received  a  copy of the GNU General Public License
## along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
## Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
## 02110-1301, USA.
##

######

##
## Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
## Author: Likai Liu (liulk AT cs DOT bu DOT edu)
## Author: Yuri D'Elia (wavexx AT thregr DOT org)
##

.SUFFIXES:
all:: Makefile

######

# integration with autoconf.

SRC_CONFIG_FILES := config.mk.in config.h.in
BUILT_CONFIG_FILES := $(SRC_CONFIG_FILES:%.in=%)

Makefile: config.mk

config.status: configure
	./configure
	touch $@

$(BUILT_CONFIG_FILES): %: config.status $(filter-out %,$(SRC_CONFIG_FILES))
	./config.status
	touch $(BUILT_CONFIG_FILES)

config.h.in: configure.ac
	autoheader $<
	touch $@

configure: configure.ac $(SRC_CONFIG_FILES)
	aclocal
	automake --add-missing --foreign || true
	autoconf
	touch $(BUILT_CONFIG_FILES)

-include config.mk

######

DESTDIR :=

export ATSHOME
export ATSHOMERELOC
export ATSHOMEQ

######

#
# HX-2011-06-01:
# This part is by Georg Bauhaus (bauhaus AT arcor DOT de):
# It addresses the issue that the [-D] option is not available
# for $(INSTALL) on MacOSX.
#
.PHONY: install_dirs

install_dirs:: config.h
#
# make all subdirectories needed by install.
#
	cd $(abs_top_srcdir)
	[ -d $(DESTDIR)/$(bindir) ] || $(MKDIR_P) $(DESTDIR)/$(bindir)
	$(MKDIR_P) $(DESTDIR)$(ATSNEWHOME)/bin
	find ccomp contrib doc libats libc prelude -type d \
	  -exec $(MKDIR_P) $(DESTDIR)$(ATSNEWHOME)/\{} \; \
	  -print

install_files:: install_dirs
#
# recursively install all files in the list except .svn control files.
#
	for d in ccomp/runtime contrib doc libats libc prelude; do \
	  cd $(abs_top_srcdir) && \
	  $(INSTALL) -d $(DESTDIR)$(ATSNEWHOME)/"$$d" && \
	  find "$$d" -name .svn -prune -o -type f \
            -exec $(INSTALL) -m 644 \{} $(DESTDIR)$(ATSNEWHOME)/\{} \; \
	    -print && \
	  find "$$d" -name .svn -prune -o -type l \
            -exec cp -d \{} $(DESTDIR)$(ATSNEWHOME)/\{} \; \
	    -print; \
	done
#
# install specific files in the list as regular files.
#
	for f in \
	    COPYING INSTALL *.txt ccomp/lib/*.a ccomp/lib64/*.a config.h; \
	do \
	  [ -f "$$f" ] || continue; \
	  cd $(abs_top_srcdir) && \
	  $(INSTALL) -m 644 "$$f" $(DESTDIR)$(ATSNEWHOME)/"$$f" && \
	  echo "$$f"; \
	done
#
# install specific files in the list as executables.
#
	for f in bin/*; do \
	  cd $(abs_top_srcdir) && \
	  $(INSTALL) -m 755 "$$f" $(DESTDIR)$(ATSNEWHOME)/"$$f" && \
	  echo "$$f"; \
	done
#
# install multiple copies of wrapper script, for each binary.
#
	for f in bin/*; do \
	  b=`basename "$$f"`; \
	  cd $(abs_top_srcdir) && \
	  $(INSTALL) -m 755 ats_env.sh $(DESTDIR)$(bindir)/"$$b" && \
	  echo install ats_env.sh to $(bindir)/"$$b"; \
	done

install:: install_files

######

all:: \
  atsopt1 \
  bin/atscc \
  bin/atslib \
  libfiles libfiles_mt \
  bin/atspack \
  bin/atslex \
  ccomp/runtime/GCATS/gc.o \
  ccomp/runtime/GCATS/gc_mt.o \
  atsopt1_gc \
  contrib
	@echo "ATS/Anairiats has been built up successfully!"
	@echo "The value of ATSHOME for this build is \"$(ATSHOME)\"."
	@echo "The value of ATSHOMERELOC for this build is \"$(ATSHOMERELOC)\"."

###### w/o GC ######

atsopt1::
	$(MAKE) -C bootstrap1 -f ../Makefile_bootstrap BOOTSTRAP1=1 atsopt
	cp bootstrap1/atsopt $(ATSHOMEQ)/bin/atsopt

###### with GC ######

atsopt1_gc::
	$(MAKE) -C bootstrap1 -f ../Makefile_bootstrap BOOTSTRAP1=1 atsopt_gc
	cp bootstrap1/atsopt_gc $(ATSHOMEQ)/bin/atsopt

###### contrib libraries ######

contrib::
ifeq (1,1)
	$(MAKE) -C contrib/parcomb all
endif
ifeq ($(HAVE_LIBGLIB20),1)
	$(MAKE) -C contrib/glib all
endif
ifeq ($(HAVE_LIBGTK20),1)
	$(MAKE) -C contrib/cairo all
	$(MAKE) -C contrib/pango all
	$(MAKE) -C contrib/GTK all
endif
ifeq ($(HAVE_LIBSDL10),1)
	$(MAKE) -C contrib/SDL all
endif

###### some toplevel commands ######

bin/atscc bin/atslib:
	$(MAKE) -C utils/scripts atscc
	$(CPF) utils/scripts/atscc $(ATSHOMEQ)/bin
	$(MAKE) -C utils/scripts atslib
	$(CPF) utils/scripts/atslib $(ATSHOMEQ)/bin
	$(MAKE) -C utils/scripts clean

bin/atspack:
	$(MAKE) -C utils/scripts atspack
	cp utils/scripts/atspack $(ATSHOMEQ)/bin

###### library ######

ATS_PROOFCHECK=
#
# ATS_PROOFCHECK=-D_ATS_PROOFCHECK # it should be turned on from time to time
#
# [CC -E] for preprocessing
#

ATSLIB=$(ATSHOMEQ)/bin/atslib

.libfiles_local: .libfiles ; $(CC) -E -P -x c -o $@ $<
libfiles: .libfiles_local
	$(ATSLIB) $(ATS_PROOFCHECK) -D_XOPEN_SOURCE -O2 --libats
	$(ATSLIB) $(ATS_PROOFCHECK) -O2 --libats_lex
	$(ATSLIB) $(ATS_PROOFCHECK) -O2 --libats_smlbas

lib32files: .libfiles_local
	$(ATSLIB) $(ATS_PROOFCHECK) -m32 -D_XOPEN_SOURCE -O2 --libats
	$(ATSLIB) $(ATS_PROOFCHECK) -m32 -O2 --libats_lex
	$(ATSLIB) $(ATS_PROOFCHECK) -m32 -O2 --libats_smlbas

lib64files: .libfiles_local
	$(ATSLIB) $(ATS_PROOFCHECK) -m64 -D_XOPEN_SOURCE -O2 --libats
	$(ATSLIB) $(ATS_PROOFCHECK) -m64 -O2 --libats_lex
	$(ATSLIB) $(ATS_PROOFCHECK) -m64 -O2 --libats_smlbas

.libfiles_mt_local: .libfiles_mt ; $(CC) -E -P -x c -o $@ $<
libfiles_mt: .libfiles_mt_local
	$(ATSLIB) $(ATS_PROOFCHECK) -D_ATS_MULTITHREAD -O2 --libats_mt

###### a lexer for ATS ######

bin/atslex:
	cd utils/atslex; $(MAKE) atslex; cp atslex $(ATSHOMEQ)/bin
	cd utils/atslex; $(MAKE) clean

###### GC runtime ######

ccomp/runtime/GCATS/gc.o:
	cd ccomp/runtime/GCATS; $(MAKE) gc.o; $(MAKE) clean

ccomp/runtime/GCATS/gc_mt.o:
	cd ccomp/runtime/GCATS; $(MAKE) gc_mt.o; $(MAKE) clean

######

CPF=cp -f
RMF=rm -f

######

clean::
	$(RMF) bootstrap1/*.o
	$(MAKE) -C utils/scripts clean
	$(MAKE) -C utils/atslex clean
	$(MAKE) -C ccomp/runtime/GCATS clean

cleanall::
	$(RMF) bootstrap1/*.o
	$(RMF) $(BUILT_CONFIG_FILES)
	$(RMF) .libfiles_local
	$(RMF) .libfiles_mt_local
	$(RMF) bin/atsopt bin/atscc bin/atslib bin/atslex bin/atspack
	$(RMF) ccomp/lib/libats.a
	$(RMF) ccomp/lib/libats_mt.a
	$(RMF) ccomp/lib/libats_lex.a
	$(RMF) ccomp/lib/libats_smlbas.a
	$(RMF) ccomp/lib/output/*
	$(RMF) ccomp/lib64/libats.a
	$(RMF) ccomp/lib64/libats_mt.a
	$(RMF) ccomp/lib64/libats_lex.a
	$(RMF) ccomp/lib64/libats_smlbas.a
	$(RMF) ccomp/lib64/output/*
	$(RMF) contrib/glib/atsctrb_glib.o
	$(RMF) contrib/cairo/atsctrb_cairo.o
	$(RMF) contrib/pango/atsctrb_pango.o
	$(RMF) contrib/X11/atsctrb_X11.o
	$(RMF) contrib/GTK/atsctrb_GTK.o
	$(RMF) contrib/GL/atsctrb_GL.o
	$(RMF) contrib/GL/atsctrb_glut.o
	$(RMF) contrib/GL/atsctrb_gtkglext.o
	$(RMF) contrib/SDL/atsctrb_SDL.o

cleanall:: ; $(MAKE) -C utils/atslex cleanall
cleanall:: ; $(MAKE) -C utils/scripts cleanall
cleanall:: ; $(MAKE) -C ccomp/runtime/GCATS cleanall

distclean:: cleanall
distclean:: ; find . -name .svn -prune -o -name \*~ -exec rm \{} \;

######
#
# end of [Makefile_dist]
#
######
