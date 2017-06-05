
INCLUDES = \
	-I$(top_builddir) \
	-I$(top_srcdir) \
	-I$(top_srcdir)/pkcs11 \
	$(GOBJECT_CFLAGS) \
	$(GLIB_CFLAGS) \
	-DGCK_API_SUBJECT_TO_CHANGE

# ------------------------------------------------------------------------------
# The ssh-agent component code

noinst_LTLIBRARIES = \
	libgkd-ssh-agent.la

libgkd_ssh_agent_la_SOURCES = \
	gkd-ssh-agent.c gkd-ssh-agent.h \
	gkd-ssh-agent-private.h \
	gkd-ssh-agent-ops.c \
	gkd-ssh-agent-proto.c

# ------------------------------------------------------------------------------
# Standalone binary

noinst_PROGRAMS = \
	gkd-ssh-agent-standalone

gkd_ssh_agent_standalone_SOURCES = \
	gkd-ssh-agent-standalone.c

gkd_ssh_agent_standalone_LDADD = \
	libgkd-ssh-agent.la \
	$(top_builddir)/gck/libgck-@GCK_MAJOR@.la \
	$(top_builddir)/egg/libegg-buffer.la \
	$(top_builddir)/egg/libegg-secure.la \
	$(GOBJECT_LIBS) \
	$(GTHREAD_LIBS) \
	$(LIBGCRYPT_LIBS) \
	$(GLIB_LIBS)
