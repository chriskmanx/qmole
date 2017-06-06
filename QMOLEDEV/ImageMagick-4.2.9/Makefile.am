#
#  Makefile for building ImageMagick utilities
#
#  Copyright 1999 E. I. du Pont de Nemours and Company
#
#  Permission is hereby granted, free of charge, to any person obtaining a
#  copy of this software and associated documentation files ("ImageMagick"),
#  to deal in ImageMagick without restriction, including without limitation 
#  the rights to use, copy, modify, merge, publish, distribute, sublicense, 
#  and/or sell copies of ImageMagick, and to permit persons to whom the 
#  ImageMagick is furnished to do so, subject to the following conditions:
#   
#  The above copyright notice and this permission notice shall be included in 
#  all copies or substantial portions of ImageMagick. 
#   
#  The software is provided "as is", without warranty of any kind, express or 
#  implied, including but not limited to the warranties of merchantability, 
#  fitness for a particular purpose and noninfringement.In no event shall 
#  E. I. du Pont de Nemours and Company be liable for any claim, damages or 
#  other liability, whether in an action of contract, tort or otherwise,
#  arising from, out of or in connection with ImageMagick or the use or other 
#  dealings in ImageMagick. 
#   
#  Except as contained in this notice, the name of the E. I. du Pont de 
#  Nemours and Company shall not be used in advertising or otherwise to 
#  promote the sale, use or other dealings in ImageMagick without prior 
#  written authorization from the E. I. du Pont de Nemours and Company. 
#
#  This file is currently maintained by Bob Friesenhahn,
#  bfriesen@simple.dallas.tx.us

# Don't require all the GNU mandated files
# Remove comment from no-dependencies for distributions compatable with
# traditional 'make'
AUTOMAKE_OPTIONS = 1.2 foreign # no-dependencies

MAGICK_LIB	= magick/libMagick.la # libtool library name
INCLUDES	= -I$(srcdir) $(X_CFLAGS)

# Subdirectories to build
SUBDIRS = delegates magick xtp

STDMANPAGES = ImageMagick.1  combine.1 convert.1  identify.1 \
   miff.4 mogrify.1 montage.1 quantize.5
X11MANPAGES = animate.1 display.1 import.1
ALLMANPAGES = $(STDMANPAGES) $(X11MANPAGES)

# Executables to build
if HasX11
X11PROGRAMS = animate display import
MANPAGES = $(ALLMANPAGES)
else
X11PROGRAMS =
MANPAGES = $(STDMANPAGES)
endif
bin_PROGRAMS = combine convert identify mogrify montage $(X11PROGRAMS)

# Per-executable sources
display_SOURCES  = display.c
animate_SOURCES  = animate.c
import_SOURCES   = import.c
montage_SOURCES  = montage.c
convert_SOURCES  = convert.c
mogrify_SOURCES  = mogrify.c
identify_SOURCES = identify.c
combine_SOURCES  = combine.c

# Libtool libraries used by all executables
LDADD		 = $(MAGICK_LIB)

# Manual pages to install
man_MANS = $(MANPAGES)

# Additional files to distribute
EXTRA_DIST = $(ALLMANPAGES) Magick.tmpl Manifest.ps ImageMagick.html Imakefile \
	Magickshr.opt Make.com README.txt Copyright.txt FlashPix.txt \
	acconfig.h QuickStart.txt

# Non-Automake subdirectories to distribute
DISTDIRS = images scenes scripts www PerlMagick Magick++
dist-hook:
	( \
	  builddir=`pwd` ; \
	  cd $(srcdir) && \
	  ( \
	    for dir in $(DISTDIRS) ; do \
	      find $$dir -depth -print | egrep -v '(~$$)' | cpio -pdum $$builddir/$(distdir) 2> /dev/null ; \
	    done \
	  ) \
	)

if WITH_PERL

PERLMAGICK=PerlMagick
PERLMAKEMAKER=$(PERLMAGICK)/Makefile.PL
PERLMAKEFILE=$(PERLMAGICK)/Makefile

# If source files missing, see if they can be obtained via VPATH
perl-sources:
	touch perl-sources && \
	cd $(PERLMAGICK) && \
	( \
	  if test -n "$(VPATH)" ; then \
	    for file in "$(VPATH)/$(PERLMAGICK)/"* ; do \
	      base=`basename $$file` ; \
	      if test ! -r $$base ; then \
	        @LN_S@ $$file . ; \
	      fi \
	    done \
	  fi ; \
	)

if WITH_PERL_DYNAMIC

$(PERLMAKEFILE): perl-sources $(PERLMAKEMAKER)
	cd $(PERLMAGICK) && @PERL@ Makefile.PL

# Make all PERL sub-targets except those that require a link
# since the link must be against installed libraries
PERLALLTARGETS=config pm_to_blib subdirs Magick.o manifypods
all-perl: $(PERLMAKEFILE)
	cd $(PERLMAGICK) && $(MAKE) CC='@CC@' $(PERLALLTARGETS)

install-exec-perl: all-perl
	cd $(PERLMAGICK) && $(MAKE) CC='@CC@' install

else
if WITH_PERL_STATIC

PERLSTATICNAME=PerlMagick

$(PERLMAKEFILE): perl-sources $(PERLMAKEMAKER)
	cd $(PERLMAGICK) && @PERL@ Makefile.PL MAP_TARGET=$(PERLSTATICNAME)

all-perl: $(PERLMAKEFILE)
	cd $(PERLMAGICK) && $(MAKE) CC='@CC@' $(PERLSTATICNAME)

install-exec-perl: all-perl
	cd $(PERLMAGICK) && $(MAKE) -f Makefile.aperl CC='@CC@' inst_perl

endif # WITH_PERL_STATIC
endif # WTIH_PERL_DYNAMIC

check-perl: $(PERLMAKEFILE)
	cd $(PERLMAGICK) && $(MAKE) CC='@CC@' test

clean-perl: $(PERLMAKEFILE)
	cd $(PERLMAGICK) && $(MAKE) CC='@CC@' clean && rm -f  Makefile.old PerlMagick
	rm -f perl-sources

distclean-perl: clean-perl


else
# Satisfy makefile requirements if not building PERL
all-perl:
install-exec-perl:
check-perl:
clean-perl:
distclean-perl:
endif # WITH_PERL

all-local: all-perl

# Install PerlMagick
install-exec-local: install-exec-perl

# Do a make clean in PerlMagick directory 
clean-local: clean-perl

# Do a make distclean in PerlMagick directory 
distclean-local: distclean-perl

# Maintainer clean is just like 'distclean' for PerlMagick
maintainer-clean-local: distclean-local

# Run tests on PerlMagick
check-local: check-perl
