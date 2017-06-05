## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = 1.6

# The name of the module, e.g. 'glib'.
DOC_MODULE=libgnomecanvas

# The top-level SGML file. Change it if you want.
DOC_MAIN_SGML_FILE=$(DOC_MODULE)-docs.sgml

# Extra options to supply to gtkdoc-scan.
SCAN_OPTIONS=

# The directory containing the source code. Relative to $(srcdir).
# gtk-doc will search all .c & .h files beneath here for inline comments
# documenting functions and macros.
DOC_SOURCE_DIR=../../libgnomecanvas

# Used for dependencies.
HFILE_GLOB=$(top_srcdir)/libgnomecanvas/*.h
CFILE_GLOB=$(top_srcdir)/libgnomecanvas/*.c

# Header files to ignore when scanning.
IGNORE_HFILES=gnome-canvas-shape-private.h gnome-canvas-i18n.h \
  gnome-canvas-marshal.h gailcanvas.h gailcanvasitem.h gailcanvaswidget.h \
  gailcanvasgroup.h gailcanvasgroupfactory.h gailcanvasitemfactory.h \
  gailcanvastextfactory.h gailcanvaswidgetfactory.h gailcanvastext.h

# CFLAGS and LDFLAGS for compiling scan program. Only needed if your app/lib
# contains GtkObjects/GObjects and you want to document signals and properties.
INCLUDES = -I$(top_srcdir) $(LIBGNOMECANVAS_CFLAGS)
GTKDOC_LIBS = $(top_builddir)/libgnomecanvas/libgnomecanvas-2.la \
  $(LIBGNOMECANVAS_LIBS)

# Extra options to supply to gtkdoc-mkdb.
MKDB_OPTIONS=--sgml-mode --output-format=xml

# Extra SGML files that are included by $(DOC_MAIN_SGML_FILE).
content_files =

# Images to copy into HTML directory.
HTML_IMAGES =

# Extra options to supply to gtkdoc-fixref.
FIXXREF_OPTIONS=

include $(top_srcdir)/gtk-doc.make
