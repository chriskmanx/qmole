## Process this file with automake to produce Makefile.in
AUTOMAKE_OPTIONS = 1.6

# The name of the module
DOC_MODULE=libsoup-2.4

# The top-level SGML file.
DOC_MAIN_SGML_FILE=$(DOC_MODULE)-docs.sgml

# The directory containing the source code. Relative to $(srcdir).
# gtk-doc will search all .c & .h files beneath here for inline comments
# documenting functions and macros.
DOC_SOURCE_DIR=../../libsoup

# Extra options to supply to gtkdoc-scan.
SCAN_OPTIONS=

# Extra options to supply to gtkdoc-scangobj.
SCANGOBJ_OPTIONS=--type-init-func 'g_type_init(); g_thread_init(NULL)'

# Extra options to supply to gtkdoc-mkdb.
MKDB_OPTIONS=--sgml-mode --output-format=xml

# Extra options to supply to gtkdoc-fixref.
FIXXREF_OPTIONS=

# Used for dependencies.
HFILE_GLOB=
CFILE_GLOB=

# Header files to ignore when scanning.
IGNORE_HFILES= soup.h soup-marshal.h \
	soup-message-private.h soup-session-private.h \
	soup-types.h soup-enum-types.h \
	soup-auth-basic.h soup-auth-digest.h soup-auth-ntlm.h \
	soup-connection.h soup-connection-ntlm.h \
	soup-dns.h soup-auth-manager.h soup-auth-manager-ntlm.h \
	soup-message-queue.h soup-path-map.h soup-ssl.h

# Images to copy into HTML directory.
HTML_IMAGES = 

# Extra XML files that are included by $(DOC_MAIN_SGML_FILE).
content_files = client-howto.xml server-howto.xml porting-2.2-2.4.xml

# Other files to distribute.
extra_files =


# CFLAGS and LDFLAGS for compiling scan program. Only needed
# if $(DOC_MODULE).types is non-empty.
GTKDOC_CFLAGS =				\
	-I$(top_srcdir)			\
	$(GLIB_CFLAGS)			\
	$(XML_CFLAGS)			\
	$(GNUTLS_CFLAGS)

GTKDOC_LIBS = $(top_builddir)/libsoup/libsoup-$(SOUP_API_VERSION).la

# include common portion ...
include $(top_srcdir)/gtk-doc.make

# kludges
tmpl/*.sgml:

clean: clean-am
	rm -rf tmpl
