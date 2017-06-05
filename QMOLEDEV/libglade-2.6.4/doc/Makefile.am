AUTOMAKE_OPTIONS = 1.6
## Process this file with automake to produce Makefile.in
# The name of the module.
DOC_MODULE = libglade

content_files=version.xml
HTML_IMAGES=

# The top-level SGML file.
DOC_MAIN_SGML_FILE = libglade-docs.xml

#SCANGOBJ_OPTIONS =
SCAN_OPTIONS = --deprecated-guards="LIBGLADE_DISABLE_DEPRECATED"

DOC_SOURCE_DIR = ../glade
HFILE_GLOB = $(top_srcdir)/glade/*.h
CFILE_GLOB = $(top_srcdir)/glade/*.c
IGNORE_HFILES = glade-private.h

INCLUDES = -I$(top_srcdir) $(LIBGLADE_CFLAGS)
GTKDOC_LIBS = ../glade/libglade-2.0.la $(LIBGLADE_LIBS)

include $(top_srcdir)/gtk-doc.make

# Other files to distribute
EXTRA_DIST += version.xml.in
