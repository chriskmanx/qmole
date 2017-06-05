if PLATFORM_WIN32
no_undefined = -no-undefined
endif

#Headers files to be distributed
INSTALL_DIR_NAME=@PACKAGE@-@LIBCROCO_MAJOR_VERSION@.@LIBCROCO_MINOR_VERSION@
crocoincdir=$(includedir)/$(INSTALL_DIR_NAME)/libcroco
crocoinc_HEADERS= libcroco.h \
cr-additional-sel.h \
cr-attr-sel.h \
cr-cascade.h \
cr-declaration.h \
cr-doc-handler.h \
cr-enc-handler.h \
cr-input.h \
cr-num.h \
cr-om-parser.h \
cr-parser.h \
cr-pseudo.h \
cr-rgb.h \
cr-selector.h \
cr-simple-sel.h \
cr-statement.h \
cr-stylesheet.h \
cr-term.h \
cr-tknzr.h \
cr-token.h \
cr-utils.h \
cr-fonts.h \
cr-sel-eng.h \
cr-style.h \
cr-prop-list.h \
cr-parsing-location.h \
cr-string.h \
libcroco-config.h


#the files to be compiled
SRCS=\
cr-utils.c \
cr-utils.h \
cr-input.c \
cr-input.h \
cr-enc-handler.c \
cr-enc-handler.h \
cr-num.c \
cr-num.h \
cr-rgb.c \
cr-rgb.h \
cr-token.c \
cr-token.h \
cr-tknzr.c \
cr-tknzr.h \
cr-term.c \
cr-term.h \
cr-attr-sel.c \
cr-attr-sel.h \
cr-pseudo.c \
cr-pseudo.h \
cr-additional-sel.c \
cr-additional-sel.h \
cr-simple-sel.c \
cr-simple-sel.h \
cr-selector.c \
cr-selector.h \
cr-doc-handler.c \
cr-doc-handler.h \
cr-parser.c \
cr-parser.h \
cr-declaration.c \
cr-declaration.h \
cr-statement.c \
cr-statement.h \
cr-stylesheet.c \
cr-stylesheet.h \
cr-cascade.c \
cr-cascade.h \
cr-om-parser.c \
cr-om-parser.h \
cr-style.c \
cr-style.h \
cr-sel-eng.c \
cr-sel-eng.h \
cr-fonts.c \
cr-fonts.h \
cr-prop-list.c \
cr-prop-list.h \
cr-parsing-location.c \
cr-parsing-location.h \
cr-string.c \
cr-string.h

lib_LTLIBRARIES=libcroco-0.6.la
libcroco_0_6_la_SOURCES= $(SRCS)

INCLUDES=-I$(top_srcdir) -I$(top_srcdir)/intl \
-I $(top_builddir)/src @GLIB2_CFLAGS@ @LIBXML2_CFLAGS@
libcroco_0_6_la_LDFLAGS=-version-info @LIBCROCO_VERSION_INFO@ \
@GLIB2_CFLAGS@ @GLIB2_LIBS@ @LIBXML2_LIBS@ $(no_undefined)
libcroco_0_6_la_LIBADD=@GLIB2_LIBS@ @LIBXML2_LIBS@
