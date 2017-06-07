MAKEINFOFLAGS = --enable-encoding

TEXINFO_TEX=../pt/texinfo.tex

langsdir = /pt.utf8
origlangsdir = ../pt

if USE_RECODE
    urecode=true
endif

fcharset = ISO-8859-1
tcharset = UTF-8

fhtmlcharset = iso-8859-1
thtmlcharset = utf-8

maxima.texi: $(origlangsdir)/maxima.texi
	rm -f *.texi
	cp -p $(origlangsdir)/*.texi .

maxima-index.lisp: maxima.info ../build_index.pl
	perl ../build_index.pl maxima.info ':utf8' > maxima-index.lisp

maxima.html: maxima.texi
	rm -f maxima*.html 2>/dev/null
	perl ../texi2html -split_chapter --lang=pt --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 
	for f in maxima*.html; do \
	    if test x$(urecode) = xtrue ; then \
	        recode $(fcharset)..$(tcharset) $$f ; \
	    else \
	        rm -f foo.$$f 2>/dev/null ; \
	        iconv -f $(fcharset) -t $(tcharset) $$f > foo.$$f ; \
	        mv -f foo.$$f $$f ; \
	    fi; \
	done
	for f in maxima*.html; do \
	    rm -f foo.$$f 2>/dev/null ; \
	    sed -e "s|charset=$(fhtmlcharset)|charset=$(thtmlcharset)|" < $$f > foo.$$f ; \
	    mv -f foo.$$f $$f ; \
	done

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/pt.utf8
genericdirDATA = \
contents.hhc index.hhk header.hhp

EXTRA_DIST = maxima-index.lisp $(genericdirDATA)

include ../common-lang-recode.mk

