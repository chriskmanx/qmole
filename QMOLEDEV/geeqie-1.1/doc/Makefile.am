## Process this file with automake to produce Makefile.in.

helpdir = @htmldir@

# empty help_DATA just installs the directory
help_DATA = 

EXTRA_DIST = docbook2html.sh docbook

html/GuideIndex.html: docbook/GuideIndex.xml
	rm -rf html; mkdir html
	if [ -x "$(GNOME_DOC_TOOL)" ]; then \
		"$(GNOME_DOC_TOOL)" html -o html $(srcdir)/docbook/GuideIndex.xml ; \
	else \
		echo "gnome-doc-tool not found, html is not built" ; \
	fi

html: html/GuideIndex.html

install-data-hook: html
	if [ -x "$(GNOME_DOC_TOOL)" ]; then \
		cd html; for f in * ; do $(INSTALL_DATA) "$$f" "$(DESTDIR)$(helpdir)/$$f" ; done; \
		ln -s GuideIndex.html "$(DESTDIR)$(helpdir)/index.html" ; \
	fi
	
uninstall-hook:
	rm "$(DESTDIR)$(helpdir)/"*

clean-local:
	rm -rf html

