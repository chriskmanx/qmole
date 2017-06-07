
all-local: maxima.info maxima-index.lisp maxima.html contents.hhc

maxima.info: maxima.texi
	@rm -f maxima.info* 2>/dev/null
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I $(srcdir) maxima.texi
	for f in $@ $@-[0-9] $@-[0-9][0-9]; do \
	    if test -f $$f; then \
		if test x$(urecode) = xtrue ; then \
		    recode $(fcharset)..$(tcharset) $$f ; \
		else \
		    rm -f foo.$$f 2>/dev/null ; \
		    iconv -f $(fcharset) -t $(tcharset) $$f > foo.$$f ; \
		    mv -f foo.$$f $$f ; \
		fi; \
	    fi; \
	done

contents.hhc: maxima.html
	perl ../create_index


install-data-local: install-maxima-info install-maxima-html

install-maxima-info: maxima.info maxima-index.lisp
	test -z "$(infodir)$(langsdir)" || mkdir -p -- "$(DESTDIR)$(infodir)$(langsdir)"
	@srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`; \
	list='./maxima.info'; \
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

install-maxima-html: maxima.html
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(dochtmldir)$(langsdir)
	@srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`; \
	list="$(srcdir)/maxima.html $(srcdir)/maxima_*.html" ; \
	for p in $$list; do \
	  f=`echo "$$p" | sed "s|^$$srcdirstrip/||"`; \
	  if test -f $(srcdir)/$$f; then \
            if test ! -d `dirname $(DESTDIR)$(dochtmldir)$(langsdir)/$$f`; then \
              $(mkinstalldirs) `dirname $(DESTDIR)$(dochtmldir)$(langsdir)/$$f`; \
            fi; \
	    echo " $(INSTALL_DATA) $(srcdir)/$$f $(DESTDIR)$(dochtmldir)$(langsdir)/$$f"; \
	    $(INSTALL_DATA) $(srcdir)/$$f $(DESTDIR)$(dochtmldir)$(langsdir)/$$f; \
	  else if test -f $$f; then \
            if test ! -d `dirname $(DESTDIR)$(dochtmldir)$(langsdir)/$$f`; then \
              $(mkinstalldirs) `dirname $(DESTDIR)$(dochtmldir)$(langsdir)/$$f`; \
            fi; \
	    echo " $(INSTALL_DATA) $$f $(DESTDIR)$(dochtmldir)$(langsdir)/$$f"; \
	    $(INSTALL_DATA) $$f $(DESTDIR)$(dochtmldir)$(langsdir)/$$f; \
	  fi; fi; \
	done


uninstall-local: uninstall-maxima-info uninstall-maxima-html

uninstall-maxima-info:
	@list='./maxima.info'; \
	for file in $$list; do \
	  relfile=`echo "$$file" | sed 's|^.*/||'`; \
	  relfile_i=`echo "$$relfile" | sed 's|\.info$$||;s|$$|.i|'`; \
	  (if cd "$(DESTDIR)$(infodir)$(langsdir)"; then \
	     echo " cd '$(DESTDIR)$(infodir)$(langsdir)' && rm -f $$relfile $$relfile-[0-9] $$relfile-[0-9][0-9] $$relfile_i[0-9] $$relfile_i[0-9][0-9]"; \
	     rm -f $$relfile $$relfile-[0-9] $$relfile-[0-9][0-9] $$relfile_i[0-9] $$relfile_i[0-9][0-9]; \
	   else :; fi); \
	done
	rm -f "$(DESTDIR)$(infodir)$(langsdir)/maxima-index.lisp"

uninstall-maxima-html:
	@$(NORMAL_UNINSTALL)
	rm -f $(DESTDIR)$(dochtmldir)$(langsdir)/maxima.html 
	rm -f $(DESTDIR)$(dochtmldir)$(langsdir)/maxima_*.html


clean-local: clean-info clean-html clean-texi

clean-info:
	rm -f maxima.info*
	rm -f maxima-index.lisp

clean-html:
	rm -f maxima.html maxima_*.html
	rm -f contents.hhc
	rm -f index.hhk

clean-texi:
	rm -f *.texi


dist-hook: dist-maxima-info dist-maxima-html 

dist-maxima-html: maxima.html
	@srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`; \
	list="$(srcdir)/maxima.html $(srcdir)/maxima_*.html" ; \
	for p in $$list; do \
	  f=`echo "$$p" | sed "s|^$$srcdirstrip/||"`; \
	  test -f $(distdir)/$$f || cp -p $(srcdir)/$$f $(distdir)/$$f; \
	done

dist-maxima-info: 
	@srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`; \
	list="$(srcdir)/maxima.info*" ; \
	for p in $$list; do \
	  f=`echo "$$p" | sed "s|^$$srcdirstrip/||"`; \
	  test -f $(distdir)/$$f || cp -p $(srcdir)/$$f $(distdir)/$$f; \
	done

