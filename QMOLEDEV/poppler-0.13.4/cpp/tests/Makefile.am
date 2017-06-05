INCLUDES =					\
	-I$(top_srcdir)				\
	-I$(top_srcdir)/utils			\
	-I$(top_srcdir)/cpp

LDADDS =					\
	$(top_builddir)/cpp/libpoppler-cpp.la


noinst_PROGRAMS =				\
	poppler-dump

poppler_dump_SOURCES = \
	$(top_srcdir)/utils/parseargs.c		\
	poppler-dump.cpp
poppler_dump_LDADD = $(LDADDS)

