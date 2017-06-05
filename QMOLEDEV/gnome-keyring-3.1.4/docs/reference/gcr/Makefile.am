## Process this file with automake to produce Makefile.in

# We require automake 1.6 at least.
AUTOMAKE_OPTIONS = 1.6

# This is a blank Makefile.am for using gtk-doc.
# Copy this to your project's API docs directory and modify the variables to
# suit your project. See the GTK+ Makefiles in gtk+/docs/reference for examples
# of using the various options.

# The name the module, e.g. 'glib'.
DOC_MODULE=gcr

# Uncomment for versioned docs and specify the version of the module, e.g. '2'.
DOC_MODULE_VERSION=@GCR_MAJOR@

# The top-level SGML file. You can change this if you want to.
DOC_MAIN_SGML_FILE=$(DOC_MODULE)-docs.sgml

# The directory containing the source code. Relative to $(srcdir).
# gtk-doc will search all .c & .h files beneath here for inline comments
# documenting the functions and macros.
# e.g. DOC_SOURCE_DIR=../../../gtk
DOC_SOURCE_DIR=../../../$(DOC_MODULE)

# Extra options to pass to gtkdoc-scangobj. Not normally needed.
SCANGOBJ_OPTIONS=

# Extra options to supply to gtkdoc-scan.
# e.g. SCAN_OPTIONS=--deprecated-guards="GTK_DISABLE_DEPRECATED"
SCAN_OPTIONS=--deprecated-guards="GCR_DISABLE_DEPRECATED"

# Extra options to supply to gtkdoc-mkdb.
# e.g. MKDB_OPTIONS=--sgml-mode --output-format=xml
MKDB_OPTIONS=--sgml-mode --output-format=xml

# Extra options to supply to gtkdoc-mktmpl
# e.g. MKTMPL_OPTIONS=--only-section-tmpl
MKTMPL_OPTIONS=

# Extra options to supply to gtkdoc-mkhtml
MKHTML_OPTIONS=

# Extra options to supply to gtkdoc-fixref. Not normally needed.
# e.g. FIXXREF_OPTIONS=--extra-dir=../gdk-pixbuf/html --extra-dir=../gdk/html
FIXXREF_OPTIONS=

# Used for dependencies. The docs will be rebuilt if any of these change.
# e.g. HFILE_GLOB=$(top_srcdir)/gtk/*.h
# e.g. CFILE_GLOB=$(top_srcdir)/gtk/*.c
HFILE_GLOB=$(top_srcdir)/$(DOC_MODULE)/*.h
CFILE_GLOB=$(top_srcdir)/$(DOC_MODULE)/*.c

# Extra header to include when scanning, which are not under DOC_SOURCE_DIR
# e.g. EXTRA_HFILES=$(top_srcdir}/contrib/extra.h
EXTRA_HFILES=

# Header files to ignore when scanning. Use base file name, no paths
# e.g. IGNORE_HFILES=gtkdebug.h gtkintl.h
IGNORE_HFILES= \
	gcr-certificate-exporter.h \
	gcr-certificate-basics-widget.h \
	gcr-certificate-details-widget.h \
	gcr-display-scrolled.h \
	gcr-display-view.h \
	gcr-icons.h \
	gcr-import-dialog.h \
	gcr-internal.h \
	gcr-live-search.h \
	gcr-marshal.h \
	gcr-xxx.h \
	gcr-zzz.h

# Images to copy into HTML directory.
# e.g. HTML_IMAGES=$(top_srcdir)/gtk/stock-icons/stock_about_24.png
HTML_IMAGES= \
	$(srcdir)/images/certificate-widget.png \
	$(srcdir)/images/key-widget.png \
	$(srcdir)/images/combo-selector.png \
	$(srcdir)/images/tree-selector.png \
	$(srcdir)/images/list-selector.png

# Extra SGML files that are included by $(DOC_MAIN_SGML_FILE).
# e.g. content_files=running.sgml building.sgml changes-2.0.sgml
content_files=gcr-visual-index.xml

# SGML files where gtk-doc abbrevations (#GtkWidget) are expanded
# These files must be listed here *and* in content_files
# e.g. expand_content_files=running.sgml
expand_content_files=

# CFLAGS and LDFLAGS for compiling gtkdoc-scangobj with your library.
# Only needed if you are using gtkdoc-scangobj to dynamically query widget
# signals and properties.
# e.g. GTKDOC_CFLAGS=-I$(top_srcdir) -I$(top_builddir) $(GTK_DEBUG_FLAGS)
# e.g. GTKDOC_LIBS=$(top_builddir)/gtk/$(gtktargetlib)
GTKDOC_CFLAGS= -I$(top_srcdir) -I$(top_builddir) $(GOBJECT_CFLAGS) -Wno-error
GTKDOC_LIBS= $(GOBJECT_LIBS) \
	$(top_builddir)/$(DOC_MODULE)/libgcr-@GCR_MAJOR@.la \
	$(top_builddir)/gck/libgck-@GCK_MAJOR@.la

# This includes the standard gtk-doc make rules, copied by gtkdocize.
include $(top_srcdir)/gtk-doc.make

# Other files to distribute
# e.g. EXTRA_DIST += version.xml.in
# EXTRA_DIST +=

# Files not to distribute
# for --rebuild-types in $(SCAN_OPTIONS), e.g. $(DOC_MODULE).types
# for --rebuild-sections in $(SCAN_OPTIONS) e.g. $(DOC_MODULE)-sections.txt
DISTCLEANFILES = tmpl/gcr-unused.sgml

# Comment this out if you want your docs-status tested during 'make check'
#TESTS_ENVIRONMENT = cd $(srcsrc)
#TESTS = $(GTKDOC_CHECK)

# ------------------------------------------------------------------------------
# SHOOTER

WIDGETS = \
	certificate-widget \
	key-widget \
	combo-selector \
	tree-selector \
	list-selector

shots: gcr-shooter
	mkdir -p $(builddir)/images
	for widget in $(WIDGETS); do ./gcr-shooter $$widget $(builddir)/images; done

SHOOTER_SRCS = \
	gcr-shooter.c \
	gcr-widgets.c

SHOOTER_HEADS = \
	gcr-shooter.h

gcr-shooter: $(SHOOTER_SRCS) $(SHOOTER_HEADERS) Makefile
	$(AM_V_CC) libtool --mode=link gcc -o gcr-shooter -Wall -Werror $(GTK_CFLAGS) \
		-I$(top_srcdir)/gcr -DGCR_API_SUBJECT_TO_CHANGE \
		-DSRCDIR=\"$(srcdir)\" -DTOPDIR=\"$(top_srcdir)\" \
		`pkg-config x11 xext --cflags --libs` \
		$(GTK_LDFLAGS) -lm $(top_builddir)/gcr/libgcr-testable.la \
		$(SHOOTER_SRCS)

EXTRA_DIST += $(SHOOTER_SRCS) \
	$(SHOOTER_HEADERS) \
	images