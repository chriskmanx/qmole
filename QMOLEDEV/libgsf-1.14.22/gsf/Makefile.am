AM_CPPFLAGS = -I$(top_srcdir) $(LIBGSF_CFLAGS) \
	$(Z_CPPFLAGS) $(LIBGSF_GIO_CFLAGS) \
	-DGSFLOCALEDIR=\"$(datadir)/locale\"

lib_LTLIBRARIES = libgsf-1.la

if WITH_GIO
gioc=gsf-input-gio.c gsf-output-gio.c
gioh=gsf-input-gio.h gsf-output-gio.h
giola=$(LIBGSF_GIO_LIBS)
else
gioc=
gioh=
giola=
endif


libgsf_1_la_LIBADD  = $(LIBGSF_LIBS) $(Z_LIBS) $(BZ2_LIBS) $(giola)
libgsf_1_la_LDFLAGS = -version-info $(VERSION_INFO)
if WITH_WIN32
  libgsf_1_la_DEPENDENCIES = lib.def
  libgsf_1_la_LDFLAGS += -no-undefined -export-symbols lib.def
endif

libgsf_1_la_SOURCES =		\
	gsf-utils.c		\
	gsf-libxml.c		\
	gsf-doc-meta-data.c	\
	gsf-docprop-vector.c	\
	gsf-msole-impl.h	\
	gsf-msole-utils.c	\
	gsf-open-pkg-utils.c	\
	gsf-opendoc-utils.c	\
	gsf-timestamp.c		\
	gsf-zip-impl.h		\
	gsf-zip-utils.c		\
	\
	gsf-input.c		\
	gsf-input-bzip.c	\
	gsf-input-gzip.c 	\
	gsf-input-http.c 	\
	gsf-input-iochannel.c	\
	gsf-input-memory.c 	\
	gsf-input-proxy.c 	\
	gsf-input-stdio.c 	\
	gsf-input-textline.c 	\
	\
	gsf-infile.c 		\
	gsf-infile-msole.c 	\
	gsf-infile-msvba.c 	\
	gsf-infile-stdio.c	\
	gsf-infile-tar.c	\
	gsf-infile-zip.c	\
	\
	gsf-output.c 		\
	gsf-output-bzip.c	\
	gsf-output-csv.c	\
	gsf-output-gzip.c 	\
	gsf-output-iconv.c	\
	gsf-output-iochannel.c	\
	gsf-output-memory.c	\
	gsf-output-stdio.c	\
	\
	gsf-outfile.c 		\
	gsf-outfile-msole.c 	\
	gsf-outfile-stdio.c	\
	gsf-outfile-zip.c	\
	\
	gsf-shared-memory.c	\
	gsf-structured-blob.c	\
	gsf-blob.c		\
	gsf-clip-data.c		\
	\
	$(gioc)			\
	version.c
#	gsf-output-transaction.c	\
#	gsf-command-context.c	\
#	gsf-io-context.c

libgsf_1_includedir = $(includedir)/libgsf-1/gsf
libgsf_1_include_HEADERS = 	\
	gsf.h			\
	gsf-utils.h		\
	gsf-libxml.h		\
	gsf-impl-utils.h	\
	gsf-doc-meta-data.h	\
	gsf-docprop-vector.h	\
	gsf-msole-utils.h	\
	gsf-open-pkg-utils.h	\
	gsf-opendoc-utils.h	\
	gsf-timestamp.h		\
	\
	gsf-input.h		\
	gsf-input-impl.h	\
	gsf-input-bzip.h	\
	gsf-input-gzip.h 	\
	gsf-input-http.h 	\
	gsf-input-iochannel.h	\
	gsf-input-memory.h 	\
	gsf-input-proxy.h 	\
	gsf-input-stdio.h 	\
	gsf-input-textline.h 	\
	\
	gsf-infile.h 		\
	gsf-infile-impl.h 	\
	gsf-infile-msole.h 	\
	gsf-infile-msvba.h 	\
	gsf-infile-stdio.h	\
	gsf-infile-tar.h	\
	gsf-infile-zip.h	\
	\
	gsf-output.h		\
	gsf-output-impl.h	\
	gsf-output-bzip.h	\
	gsf-output-csv.h	\
	gsf-output-gzip.h 	\
	gsf-output-iconv.h	\
	gsf-output-iochannel.h	\
	gsf-output-memory.h	\
	gsf-output-stdio.h	\
	\
	gsf-outfile.h 		\
	gsf-outfile-impl.h 	\
	gsf-outfile-msole.h	\
	gsf-outfile-stdio.h	\
	gsf-outfile-zip.h	\
	\
	gsf-shared-memory.h	\
	gsf-structured-blob.h   \
	gsf-meta-names.h	\
	gsf-blob.h		\
	gsf-clip-data.h		\
	\
	$(gioh)
#	gsf-command-context.h	\
#	gsf-io-context.h

include $(top_srcdir)/gsf.mk
CPP_CFLAGS = -DGLIB_CHECK_VERSION\(x,y,z\)
LIB_PUBLIC_HDRS = $(libgsf_1_include_HEADERS)

if HAVE_LIBEXE
gsf_mslibdir = $(DESTDIR)$(libdir)
gsf_mslib_DATA = gsf-1.lib

gsf-1.lib: libgsf-1.la lib.def
	lib -name:libgsf-1-$(CURRENT_MINUS_AGE).dll -def:lib.def -out:$@

CLEANFILES = gsf-1.exp gsf-1.lib
endif

