AM_CFLAGS = $(XSCRNSAVER_CFLAGS) $(MALLOC_ZERO_CFLAGS)
lib_LTLIBRARIES = libXss.la

libXss_la_SOURCES = \
         XScrnSaver.c

libXss_la_LIBADD = $(XSCRNSAVER_LIBS)

libXss_la_LDFLAGS = -version-info 1:0:0 -no-undefined

if LINT
ALL_LINT_FLAGS=$(LINT_FLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
		$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS)

lint:
	$(LINT) $(ALL_LINT_FLAGS) $(libXss_la_SOURCES)
endif LINT

if MAKE_LINT_LIB
lintlibdir = $(libdir)

lintlib_DATA = $(LINTLIB)

$(LINTLIB): $(libXss_la_SOURCES)
	$(LINT) -y -oXss -x $(ALL_LINT_FLAGS) $(libXss_la_SOURCES)
endif MAKE_LINT_LIB
