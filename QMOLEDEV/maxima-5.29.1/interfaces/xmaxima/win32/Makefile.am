# -*-mode: makefile; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: $
#
AUTOMAKE_OPTIONS = no-dependencies
CC = gcc # to supress automake warning

include $(top_srcdir)/common.mk

genericdir = $(prefix)
genericdirDATA = readme_en.txt readme_ru.txt readme_fr.txt \
  readme_de.txt readme_pt.txt readme_pt_BR.txt readme_it.txt readme_es.txt

bin_PROGRAMS = xmaxima winkill
bin_SCRIPTS = tclwinkill.dll # This is an abuse, but avoids libtool
xmaxima_SOURCES =

../xmaxima:; $(MAKE) -C .. xmaxima

xmaxima.tcl: ../xmaxima; cp ../xmaxima xmaxima.tcl

xmaxima$(EXEEXT): xmaxima.tcl
	-rm -rf xmaxima.vfs img.vfs
	$(TCLKITSH) $(SDXKIT) qwrap xmaxima.tcl && \
	$(TCLKITSH) $(SDXKIT) unwrap xmaxima.kit && \
	$(TCLKITSH) $(SDXKIT) unwrap $(IMGKIT) && \
	cp -r img.vfs/lib/Img xmaxima.vfs/lib && \
	$(TCLKITSH) $(SDXKIT) wrap xmaxima.exe \
	-runtime $(TCLKIT_RUNTIME) xmaxima.kit

winkill$(EXEEXT): winkill.c
	gcc -Wall -o $@ winkill.c

tclwinkill.dll: tclwinkill.c
	gcc -Wall -shared tclwinkill.c $(TCLSTUBLIB) -o $@

InfoBefore = InfoBefore.txt InfoBefore_ru.txt InfoBefore_fr.txt \
  InfoBefore_de.txt InfoBefore_pt.txt InfoBefore_pt_BR.txt \
  InfoBefore_it.txt InfoBefore_es.txt
EXTRA_DIST = ChangeLog $(genericdirDATA) $(InfoBefore) tclwinkill.c
CLEANFILES = tclwinkill.dll xmaxima.tcl xmaxima.kit

clean-local:; -rm -rf  xmaxima.vfs img.vfs

FORCE:
