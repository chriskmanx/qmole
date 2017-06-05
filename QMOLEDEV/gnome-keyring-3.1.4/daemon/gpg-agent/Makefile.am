
INCLUDES = \
	-I$(top_builddir) \
	-I$(top_srcdir) \
	-I$(top_srcdir)/pkcs11 \
	-DGCK_API_SUBJECT_TO_CHANGE \
	$(GOBJECT_CFLAGS) \
	$(GIO_CFLAGS) \
	$(GLIB_CFLAGS)

# ------------------------------------------------------------------------------
# The ssh-agent component code

noinst_LTLIBRARIES = \
	libgkd-gpg-agent.la

libgkd_gpg_agent_la_SOURCES = \
	gkd-gpg-agent.c gkd-gpg-agent.h \
	gkd-gpg-agent-private.h \
	gkd-gpg-agent-ops.c

# ------------------------------------------------------------------------------
# Standalone binary

noinst_PROGRAMS = \
	gkd-gpg-agent-standalone

gkd_gpg_agent_standalone_SOURCES = \
	gkd-gpg-agent-standalone.c

gkd_gpg_agent_standalone_LDADD = \
	libgkd-gpg-agent.la \
	$(top_builddir)/ui/libgku-prompt.la \
	$(top_builddir)/gck/libgck-@GCK_MAJOR@.la \
	$(top_builddir)/egg/libegg.la \
	$(GOBJECT_LIBS) \
	$(GTHREAD_LIBS) \
	$(GIO_LIBS) \
	$(LIBGCRYPT_LIBS) \
	$(GLIB_LIBS)
