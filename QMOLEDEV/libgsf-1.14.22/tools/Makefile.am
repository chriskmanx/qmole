AM_CPPFLAGS = -I$(top_srcdir) $(LIBGSF_CFLAGS) \
	$(Z_CPPFLAGS) \
	-DGSFLOCALEDIR=\"$(datadir)/locale\"
LDADD = $(top_builddir)/gsf/libgsf-1.la $(LIBGSF_LIBS)

bin_PROGRAMS = gsf gsf-vba-dump
gsf_SOURCES = gsf.c
gsf_vba_dump_SOURCES = gsf-vba-dump.c
