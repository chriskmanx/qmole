if CHM
MAXIMA_CHM = maxima.chm
INSTALL_CHM = install-chm
UNINSTALL_CHM = uninstall-chm
CLEAN_CHM = clean-chm
endif

all-local: maxima-index.lisp maxima.html contents.hhc $(MAXIMA_CHM)

install-data-local: $(INSTALL_CHM)

uninstall-local: $(UNINSTALL_CHM)

maxima-index.lisp: maxima.info ../build_index.pl
	perl ../build_index.pl maxima.info ':crlf' > maxima-index.lisp

maxima.html: maxima.texi $(maxima_TEXINFOS)
	perl ../texi2html -split_chapter --lang=$(lang) --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 

contents.hhc: maxima.html
	perl ../create_index

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/$(langsdir)
genericdirDATA = \
contents.hhc index.hhk header.hhp maxima.hhp

htmlname = maxima
htmlinstdir = $(dochtmldir)/$(langsdir)
include $(top_srcdir)/common-html.mk

clean-local: clean-info clean-html $(CLEAN_CHM)

clean-info:
	rm -f maxima.info*
	rm -f maxima-index.lisp

clean-html:
	rm -f maxima.html maxima_*.html
	rm -f contents.hhc
	rm -f index.hhk

EXTRA_DIST = maxima-index.lisp $(genericdirDATA)

# This builds the Windows help file maxima.chm
maxima.chm: maxima.html maxima.hhp contents.hhc index.hhk
	$(MKDIR_P) chm
	$(MKDIR_P) chm/figures
	for hfile in *.html ; do \
	  sed -e 's|../figures|figures|g' < $$hfile > chm/$$hfile; \
	done
	cp maxima.hhp contents.hhc index.hhk chm
	cp ../figures/*.gif chm/figures
	-(cd chm; "$(HHC)" maxima.hhp)
	mv chm/maxima.chm .

install-chm: maxima.chm
	test -z "$(DESTDIR)$(docchmdir)/$(langsdir)" || mkdir -p -- "$(DESTDIR)$(docchmdir)/$(langsdir)"
	$(INSTALL_DATA) maxima.chm "$(DESTDIR)$(docchmdir)/$(langsdir)/maxima.chm"

uninstall-chm:
	rm -f "$(DESTDIR)$(docchmdir)"

clean-chm:
	rm -f maxima.chm
	rm -rf chm


install-info-am: $(INFO_DEPS) maxima-index.lisp
	test -z "$(infodir)$(langsdir)" || mkdir -p -- "$(DESTDIR)$(infodir)$(langsdir)"
	@srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`; \
	list='$(INFO_DEPS)'; \
	for file in $$list; do \
	  case $$file in \
	    $(srcdir)/*) file=`echo "$$file" | sed "s|^$$srcdirstrip/||"`;; \
	  esac; \
	  if test -f $$file; then d=.; else d=$(srcdir); fi; \
	  file_i=`echo "$$file" | sed 's|\.info$$||;s|$$|.i|'`; \
	  for ifile in $$d/$$file $$d/$$file-[0-9] $$d/$$file-[0-9][0-9] \
                       $$d/$$file_i[0-9] $$d/$$file_i[0-9][0-9] ; do \
	    if test -f $$ifile; then \
	      relfile=`echo "$$ifile" | sed 's|^.*/||'`; \
	      echo " $(INSTALL_DATA) '$$ifile' '$(DESTDIR)$(infodir)$(langsdir)/$$relfile'"; \
	      $(INSTALL_DATA) "$$ifile" "$(DESTDIR)$(infodir)$(langsdir)/$$relfile"; \
	    else : ; fi; \
	  done; \
	done
	$(INSTALL_DATA) maxima-index.lisp "$(DESTDIR)$(infodir)$(langsdir)/maxima-index.lisp"

uninstall-info-am:
	@list='$(INFO_DEPS)'; \
	for file in $$list; do \
	  relfile=`echo "$$file" | sed 's|^.*/||'`; \
	  relfile_i=`echo "$$relfile" | sed 's|\.info$$||;s|$$|.i|'`; \
	  (if cd "$(DESTDIR)$(infodir)$(langsdir)"; then \
	     echo " cd '$(DESTDIR)$(infodir)$(langsdir)' && rm -f $$relfile $$relfile-[0-9] $$relfile-[0-9][0-9] $$relfile_i[0-9] $$relfile_i[0-9][0-9]"; \
	     rm -f $$relfile $$relfile-[0-9] $$relfile-[0-9][0-9] $$relfile_i[0-9] $$relfile_i[0-9][0-9]; \
	   else :; fi); \
	done
	rm -f "$(DESTDIR)$(infodir)$(langsdir)/maxima-index.lisp"
