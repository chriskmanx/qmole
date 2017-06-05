# programs used for doc creation
babl_fish_path_dhtml   = $(top_builddir)/tests/babl_fish_path_dhtml$(EXEEXT)
babl_fish_path_fitness = $(top_builddir)/tests/babl_fish_path_fitness$(EXEEXT)
babl_html_dump         = $(top_builddir)/tests/babl-html-dump$(EXEEXT)

.SILENT:
SUBDIRS=      graphics
DIST_SUBDIRS= graphics

# The patterns used for html creation in the babl build system
# is very small, and should probably be documented in a tutorial.
#	
#
#
HTML_FILES = index-static.html 	\
	     index.html		\
	     BablFishPath.html

EXTRA_DIST= 			\
	babl.css  		\
	tools/xml_insert.sh	\
	tools/changelog2rss	\
	index-static.html.in	\
	COPYING			\
	COPYING.LESSER

BUILT_EXTRA_DIST = index.html BablFishPath.html BablFishPath.txt

CLEANFILES = README changelog.rss
DISTCLEANFILES = index-static.html $(BUILT_EXTRA_DIST)

all: $(HTML_FILES)


BablFishPath.html: $(babl_fish_path_dhtml)
	echo -n "HTML:"
	echo -n " $@"
	export BABL_PATH="$(top_builddir)/extensions:$(top_builddir)/extensions/.libs"; $< > $@ &&\
	echo " [OK]" || echo " Fail"

BablFishPath.txt: $(babl_fish_path_fitness)
	echo -n "UTF8:"
	echo -n " $@"
	export BABL_PATH="$(top_builddir)/extensions:$(top_builddir)/extensions/.libs"; $< > $@ &&\
	echo " [OK]" || echo " Fail"
	if [ "x"$$CRUCIBLE_ID != "x" ] ;then echo;echo;cat $@;echo;echo;fi

index.html: index-static.html                    \
	    $(babl_html_dump) 			 \
	    $(top_srcdir)/AUTHORS                \
	    $(top_srcdir)/TODO                   \
	    $(top_srcdir)/NEWS                   \
	    BablFishPath.txt			 \
	    Makefile.am
	echo -n "HTML: $@"
	cp $< $@ 
	(which mktemp > /dev/null 2>&1 && TMPFILE=`mktemp` || TMPFILE="/tmp/babl_build_tempfile" ;\
	export BABL_PATH="$(top_builddir)/extensions:$(top_builddir)/extensions/.libs"; $(babl_html_dump) > $$TMPFILE;\
	$(SHELL) $(top_srcdir)/docs/tools/xml_insert.sh $@ BablBase $$TMPFILE;\
	rm -f $$TMPFILE )
	echo -n "."

	$(SHELL) $(top_srcdir)/docs/tools/xml_insert.sh $@ BablFishPath BablFishPath.txt
	$(SHELL) $(top_srcdir)/docs/tools/xml_insert.sh $@ AUTHORS $(top_srcdir)/AUTHORS
	$(SHELL) $(top_srcdir)/docs/tools/xml_insert.sh $@ TODO $(top_srcdir)/TODO
	$(SHELL) $(top_srcdir)/docs/tools/xml_insert.sh $@ NEWS $(top_srcdir)/NEWS
	echo " [OK]"

distclean-local:
	if test $(srcdir) = .; then :; else 	\
		rm -f $(BUILT_EXTRA_DIST);	\
	fi

web: all graphics
	echo -n " WEB: "
	ping -c 1 $(WEBSITE_HOST) >/dev/null && \
	   (\
	      scp *.txt *.html *.css\
	          "$(WEBSITE_HOST):$(WEBSITE_LOCATION)/" >/dev/null && \
		  echo -n "."; \
	      scp graphics/*.png graphics/*.html\
	          "$(WEBSITE_HOST):$(WEBSITE_LOCATION)/graphics/" >/dev/null\
		&&\
	          echo "[OK]"\
		||\
	          echo "FAIL   scp failed (does dirs exist?)."\
	   )\
	|| \
	   (\
	   echo "FAIL   $(WEBSITE_HOST) not reachable."\
	   )
