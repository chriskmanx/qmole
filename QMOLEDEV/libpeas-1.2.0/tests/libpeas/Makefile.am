include $(top_srcdir)/tests/Makefile.tests

SUBDIRS = introspection plugins testing

INCLUDES = \
	-I$(top_srcdir)			\
	-I$(srcdir)			\
	-I$(srcdir)/../testing-util	\
	$(PEAS_CFLAGS)			\
	$(WARN_CFLAGS)			\
	$(DISABLE_DEPRECATED)

noinst_PROGRAMS = $(TEST_PROGS)

progs_ldadd = \
	$(PEAS_LIBS)				\
	$(top_builddir)/libpeas/libpeas-1.0.la	\
	./introspection/libintrospection-1.0.la	\
	./testing/libtesting.la			\
	../testing-util/libtesting-util.la


TEST_PROGS     += engine
engine_SOURCES  = engine.c
engine_LDADD    = $(progs_ldadd)

TEST_PROGS          += extension-c
extension_c_SOURCES  = extension-c.c
extension_c_LDADD    = $(progs_ldadd)

if ENABLE_GJS
TEST_PROGS            += extension-gjs
extension_gjs_SOURCES  = extension-gjs.c
extension_gjs_CFLAGS   = $(GJS_CFLAGS)
extension_gjs_LDADD    = $(progs_ldadd) $(GJS_LIBS)
endif

if ENABLE_PYTHON
TEST_PROGS               += extension-python
extension_python_SOURCES  = extension-python.c
extension_python_CFLAGS   = $(PYTHON_CFLAGS)
extension_python_LDADD    = $(progs_ldadd) $(PYTHON_LIBS)
endif

if ENABLE_SEED
TEST_PROGS             += extension-seed
extension_seed_SOURCES  = extension-seed.c
extension_seed_CFLAGS   = $(SEED_CFLAGS)
extension_seed_LDADD    = $(progs_ldadd)  $(SEED_LIBS)
endif

TEST_PROGS            += extension-set
extension_set_SOURCES  = extension-set.c
extension_set_LDADD    = $(progs_ldadd)

TEST_PROGS          += plugin-info
plugin_info_SOURCES  = plugin-info.c
plugin_info_LDADD    = $(progs_ldadd)
