## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = 1.6

# The name of the module.
DOC_MODULE=atk

# The top-level SGML file.
DOC_MAIN_SGML_FILE=atk-docs.sgml

# The directory containing the source code (if it contains documentation).
DOC_SOURCE_DIR=../atk

# Used for dependencies
HFILE_GLOB = $(top_srcdir)/atk/*.h
CFILE_GLOB = $(top_srcdir)/atk/*.c

# Header files to ignore when scanning
IGNORE_HFILES =	atkmarshal.h atkintl.h

# CFLAGS and LDFLAGS for compiling scan program. Only needed
# if $(DOC_MODULE).types is non-empty.
INCLUDES = 			\
	-I$(top_srcdir)		\
	-I$(top_builddir)	\
	$(DEP_CFLAGS)

GTKDOC_LIBS = $(top_builddir)/atk/libatk-$(ATK_API_VERSION).la \
              $(DEP_LIBS)

# gtkdoc-mkdb related varaibles
MKDB_OPTIONS =
content_files =		\
	version.xml
HTML_IMAGES =

include $(top_srcdir)/gtk-doc.make

# Other files to distribute
EXTRA_DIST += version.xml.in
