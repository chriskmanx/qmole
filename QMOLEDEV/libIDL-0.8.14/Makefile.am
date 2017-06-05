# -*- mode: makefile -*-
#
# libIDL Makefile.am
#
if OS_WIN32
libIDL_def = libIDL.def
export_symbols = -export-symbols libIDL.def
endif

SUBDIRS = include

DISTCHECK_CONFIGURE_FLAGS = --disable-silent-rules

INCLUDES =                             \
	-DYYDEBUG=1                    \
	-DYYERROR_VERBOSE=1            \
	-I$(top_builddir)/include      \
	$(LIBIDL_CFLAGS)               \
	-DG_LOG_DOMAIN=\"libIDL\"      \
	$(WARN_CFLAGS)

YFLAGS = -d -v 2>/dev/null

lexer.c: $(srcdir)/lexer.l parser.h
	@rm -f $@
	$(AM_V_GEN) $(LEX) $(LFLAGS) -t $(srcdir)/lexer.l | sed -e 's/yy/__IDL_/g' >$@

stamp-parser: $(srcdir)/parser.y
	$(AM_V_GEN) $(YACC) $(YFLAGS) $(srcdir)/parser.y
	@touch $@

parser.c: stamp-parser
	$(AM_V_GEN) test -f y.tab.c && mv -f y.tab.c parser.c || touch $@

parser.h: stamp-parser
	$(AM_V_GEN) test -f y.tab.h && mv -f y.tab.h parser.h || touch $@

MOSTLYCLEANFILES	= parser.output y.output

EXTRA_DIST		= BUGS parser.y lexer.l \
			  Makefile.msc README.win32 libIDL-zip.in \
			  libIDL.def \
			  HACKING MAINTAINERS

info_TEXINFOS		= libIDL2.texi
lib_LTLIBRARIES		= libIDL-2.la
noinst_PROGRAMS		= tstidl

bin_SCRIPTS		= libIDL-config-2

BUILT_SOURCES		= lexer.c parser.c parser.h stamp-parser

libIDL_2_la_SOURCES	= parser.c parser.h lexer.c \
			  ns.c util.c util.h rename.h
libIDL_2_la_CFLAGS      = -DLIBIDL_COMPILATION
libIDL_2_la_LDFLAGS	= -version-info $(LT_VERSION) $(LIBIDL_LIBS) -no-undefined $(export_symbols)
libIDL_2_la_DEPENDENCIES = $(libIDL_def)

tstidl_SOURCES		= tstidl.c
tstidl_LDADD		= libIDL-2.la $(LIBIDL_LIBS)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA= libIDL-2.0.pc

CLEANFILES = $(BUILT_SOURCES)

dist-hook:
	cd $(distdir) ; rm -f $(CLEANFILES)
