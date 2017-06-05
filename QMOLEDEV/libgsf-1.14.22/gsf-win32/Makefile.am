AM_CPPFLAGS = -I$(top_srcdir) $(LIBGSF_CFLAGS)

if WITH_WIN32

lib_LTLIBRARIES = libgsf-win32-1.la

libgsf_win32_1_la_DEPENDENCIES = lib.def
libgsf_win32_1_la_LIBADD  = $(LIBGSF_LIBS) $(top_builddir)/gsf/libgsf-1.la
libgsf_win32_1_la_LDFLAGS = -version-info $(VERSION_INFO) -no-undefined -export-symbols lib.def
libgsf_win32_1_la_SOURCES =	\
	gsf-input-win32.c	\
	gsf-output-win32.c

## But headers are installed on all systems:
libgsf_win32_1_includedir = $(includedir)/libgsf-1/gsf-win32
libgsf_win32_1_include_HEADERS =  $(gsf_win32_headers)
else
EXTRA_DIST = $(gsf_win32_headers)
endif

gsf_win32_headers = \
	gsf-input-win32.h	\
	gsf-output-win32.h

include $(top_srcdir)/gsf.mk
LIB_PUBLIC_HDRS = $(libgsf_win32_1_include_HEADERS)

if HAVE_LIBEXE
gsf_win32_mslibdir = $(DESTDIR)$(libdir)
gsf_win32_mslib_DATA = gsf-win32-1.lib

gsf-win32-1.lib: libgsf-win32-1.la lib.def
	lib -name:libgsf-win32-1-$(CURRENT_MINUS_AGE).dll -def:lib.def -out:$@

CLEANFILES = gsf-win32-1.exp gsf-win32-1.lib
endif

