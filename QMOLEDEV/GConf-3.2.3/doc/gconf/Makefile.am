## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = 1.6

# The name of the module.
DOC_MODULE=gconf

# The top-level SGML file.
DOC_MAIN_SGML_FILE=gconf-docs.sgml

# The directory containing the source code. Relative to $(srcdir)
DOC_SOURCE_DIR=../../gconf

# Extra options for gtkdoc-scangobj
OBJSCAN_OPTIONS=--nogtkinit

# Extra options to supply to gtkdoc-scan
SCAN_OPTIONS=--deprecated-guards="GCONF_ENABLE_BROKEN|GCONF_DISABLE_DEPRECATED"

# Extra options to supply to gtkdoc-mkdb
MKDB_OPTIONS=--sgml-mode --output-format=xml

# Extra options to supply to gtkdoc-fixref
FIXXREF_OPTIONS=--extra-dir=../glib/html

# Used for dependencies
HFILE_GLOB=$(top_srcdir)/gconf/*.h
CFILE_GLOB=$(top_srcdir)/gconf/*.c

# Header files to ignore when scanning
IGNORE_HFILES=

# Images to copy into HTML directory
HTML_IMAGES = 

# Extra SGML files that are included by $(DOC_MAIN_SGML_FILE)
content_files = 

# Other files to distribute
extra_files =

# CFLAGS and LDFLAGS for compiling scan program. Only needed
# if $(DOC_MODULE).types is non-empty.
GTKDOC_CFLAGS = -I$(top_srcdir) $(CFLAGS) $(DEPENDENT_CFLAGS)

GTKDOC_LIBS = $(LDFLAGS) $(DEPENDENT_LIBS) $(top_builddir)/gconf/libgconf-$(MAJOR_VERSION).la

include $(top_srcdir)/gtk-doc.make

.PHONY : dist-hook-local
