## Process this file with automake to produce Makefile.in
include $(top_srcdir)/Makefile.decl

AUTOMAKE_OPTIONS = 1.6

# The name of the module.
DOC_MODULE=gdk-pixbuf

# The top-level SGML file.
DOC_MAIN_SGML_FILE=gdk-pixbuf.xml

# Extra options to supply to gtkdoc-scan
SCAN_OPTIONS= \
	--source-dir=../../../contrib/gdk-pixbuf-xlib \
	--deprecated-guards="GDK_PIXBUF_ENABLE_BROKEN|GDK_PIXBUF_DISABLE_DEPRECATED"

# The directory containing the source code. Relative to $(srcdir)
DOC_SOURCE_DIR=../../../gdk-pixbuf

# Used for dependencies
HFILE_GLOB=$(top_srcdir)/gdk-pixbuf/*.h $(top_srcdir)/contrib/gdk-pixbuf-xlib/*.h
CFILE_GLOB=$(top_srcdir)/gdk-pixbuf/*.c $(top_srcdir)/contrib/gdk-pixbuf-xlib/*.c

# Header files to ignore when scanning
IGNORE_HFILES=				\
	pixops				\
	gdk-pixbuf-alias.h		\
	gdk-pixbuf-marshal.h		\
	gdk-pixbuf-i18n.h 		\
	gdk-pixbuf-private.h		\
	gdk-pixbuf-xlib-private.h	\
	io-gif-animation.h		\
	io-ani-animation.h		\
	xpm-color-table.h		\
	test-images.h

INCLUDES = 				\
	-I$(top_srcdir) 		\
	-I$(top_builddir) 		\
	$(GDK_PIXBUF_DEBUG_FLAGS)	\
	$(GDK_PIXBUF_DEP_CFLAGS)

GTKDOC_LIBS = \
	$(top_builddir)/gdk-pixbuf/libgdk_pixbuf-$(GDK_PIXBUF_API_VERSION).la \
	$(GDK_PIXBUF_DEP_LIBS)


# Extra options to supply to gtkdoc-mkdb
MKDB_OPTIONS= \
	--main-sgml-file=$(DOC_MAIN_SGML_FILE) 	\
	--sgml-mode 				\
	--source-dir=../../../contrib/gdk-pixbuf-xlib \
	--output-format=xml 			\
	--name-space=gdk_pixbuf

# Extra SGML files that are included by DOC_MAIN_SGML_FILE
content_files =				\
	version.xml			\
	gdk-pixbuf-from-drawables.xml	\
	gdk-pixbuf-rendering.xml	\
	gdk-pixbuf.xml			\
	gdk-pixbuf-csource.xml		\
	gdk-pixbuf-query-loaders.xml

# Images to copy into HTML directory
HTML_IMAGES = composite.png

# Extra options to supply to gtkdoc-fixref
FIXXREF_OPTIONS= --extra-dir=$(GLIB_PREFIX)/share/gtk-doc/html/gobject \
	--extra-dir=$(GLIB_PREFIX)/share/gtk-doc/html/glib


include $(top_srcdir)/gtk-doc.make

# Other files to distribute
EXTRA_DIST += version.xml.in \
	composite.png \
	composite.dia \
	apple-red-1a.png \
	apple-red-2c.png \
	gnome-gmush-1.png

########################################################################

man_MANS = 				\
	gdk-pixbuf-csource.1		\
	gdk-pixbuf-query-loaders.1

if ENABLE_MAN

XSLTPROC_FLAGS = \
        --nonet \
        --stringparam man.output.quietly 1 \
        --stringparam funcsynopsis.style ansi \
        --stringparam man.th.extra1.suppress 1 \
        --stringparam man.authors.section.enabled 0 \
        --stringparam man.copyright.section.enabled 0

.xml.1:
	$(AM_V_GEN) $(XSLTPROC) $(XSLTPROC_FLAGS) http://docbook.sourceforge.net/release/xsl/current/manpages/docbook.xsl $<

dist-local-check-mans-enabled:
	if grep "Man generation disabled" $(man_MANS) >/dev/null; then $(RM) $(man_MANS); fi

else

$(man_MANS):
	echo Man generation disabled.  Creating dummy $@.  Configure with --enable-man to enable it.
	echo Man generation disabled.  Remove this file, configure with --enable-man, and rebuild > $@


dist-local-check-mans-enabled:
	echo "*** --enable-man must be used in order to make dist"
	false

endif

MAINTAINERCLEANFILES = $(man_MANS) $(BUILT_SOURCES)

EXTRA_DIST += $(man_MANS)

dist-hook-local: dist-local-check-mans-enabled all-local

gtk-docs-clean: clean
	cd $(srcdir) && rm -rf xml html

-include $(top_srcdir)/git.mk
