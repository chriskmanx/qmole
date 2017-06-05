if OS_UNIX
CONCURRENCY_STRESS_TEST = concurrency-stress-test 
endif

TESTS =				\
	grayscale_to_rgb	\
	rgb_to_bgr       	\
	rgb_to_ycbcr		\
	srgb_to_lab_u8		\
	sanity			\
	babl_class_name		\
	types			\
	n_components		\
	models			\
	$(CONCURRENCY_STRESS_TEST)

TESTS_ENVIRONMENT = BABL_PATH=$(top_builddir)/extensions/.libs

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/babl
if OS_UNIX
AM_LDFLAGS  = -pthread
endif

LDADD = $(top_builddir)/babl/libbabl-@BABL_API_VERSION@.la \
	$(MATH_LIB)

noinst_PROGRAMS =		\
	introspect		\
	babl_fish_path_fitness	\
	babl_fish_path_dhtml	\
	babl-html-dump		\
	conversions		\
	formats			\
	nop			\
	$(TESTS)
