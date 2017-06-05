
INCLUDES = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/daemon \
	-I$(top_builddir) \
	$(DAEMON_CFLAGS) \
	$(GOBJECT_CFLAGS) \
	$(GLIB_CFLAGS) \
	-DGCK_API_SUBJECT_TO_CHANGE

noinst_LTLIBRARIES = libgkd-dbus.la

libgkd_dbus_la_SOURCES = \
	gkd-dbus.c gkd-dbus.h \
	gkd-dbus-environment.c \
	gkd-dbus-private.h \
	gkd-dbus-secrets.c \
	gkd-dbus-session.c \
	gkd-dbus-util.c gkd-dbus-util.h \
	gkd-secret-change.c gkd-secret-change.h \
	gkd-secret-create.c gkd-secret-create.h \
	gkd-secret-dispatch.c gkd-secret-dispatch.h \
	gkd-secret-error.c gkd-secret-error.h \
	gkd-secret-introspect.c gkd-secret-introspect.h \
	gkd-secret-lock.c gkd-secret-lock.h \
	gkd-secret-objects.c gkd-secret-objects.h \
	gkd-secret-property.c gkd-secret-property.h \
	gkd-secret-prompt.c gkd-secret-prompt.h \
	gkd-secret-secret.c gkd-secret-secret.h \
	gkd-secret-service.c gkd-secret-service.h \
	gkd-secret-session.c gkd-secret-session.h \
	gkd-secret-types.h \
	gkd-secret-unlock.c gkd-secret-unlock.h \
	gkd-secret-util.c gkd-secret-util.h

libgkd_dbus_la_LIBADD = \
	$(GLIB_LIBS) \
	$(GOBJECT_LIBS)

# -------------------------------------------------------------------

if WITH_TESTS
TESTS_DIR = tests
else
TESTS_DIR =
endif

SUBDIRS = . $(TESTS_DIR)
