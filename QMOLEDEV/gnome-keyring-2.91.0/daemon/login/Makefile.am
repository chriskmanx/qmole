
INCLUDES= \
	-DPREFIX=\""$(prefix)"\" \
	-DBINDIR=\""$(bindir)"\" \
	-DLIBEXECDIR=\""$(libexecdir)"\" \
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\" \
	-I$(top_srcdir) \
	-I$(top_srcdir)/daemon \
	-I$(top_builddir) \
	$(GOBJECT_CFLAGS) \
	$(GLIB_CFLAGS) \
	-DGCK_API_SUBJECT_TO_CHANGE

# ------------------------------------------------------------------
# DAEMON CODE

noinst_LTLIBRARIES = libgkd-login.la

libgkd_login_la_SOURCES = \
	gkd-login.c gkd-login.h

libgkd_login_la_LIBADD = \
	$(top_builddir)/gck/libgck.la \
	$(GOBJECT_LIBS) \
	$(GLIB_LIBS)
