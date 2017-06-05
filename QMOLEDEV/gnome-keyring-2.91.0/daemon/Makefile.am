SUBDIRS = \
	login \
	control \
	ssh-agent \
	gpg-agent \
	dbus \
	data

bin_PROGRAMS= \
	gnome-keyring-daemon

INCLUDES=	\
	-DPREFIX=\""$(prefix)"\" 			\
	-DBINDIR=\""$(bindir)"\" 			\
	-DLIBEXECDIR=\""$(libexecdir)"\"		\
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\"	\
	-I$(top_srcdir) 				\
	-I$(top_srcdir)/daemon 				\
	-I$(top_builddir) 				\
	$(DAEMON_CFLAGS)				\
	$(GOBJECT_CFLAGS)				\
	$(GTHREAD_CFLAGS)				\
	$(GIO_CFLAGS)					\
	$(GLIB_CFLAGS) 					\
	-DGCK_API_SUBJECT_TO_CHANGE

gnome_keyring_daemon_SOURCES = \
	gkd-glue.c gkd-glue.h \
	gkd-main.c gkd-main.h \
	gkd-pkcs11.c gkd-pkcs11.h \
	gkd-util.c gkd-util.h

gnome_keyring_daemon_LDADD = \
	$(top_builddir)/daemon/dbus/libgkd-dbus.la \
	$(top_builddir)/daemon/login/libgkd-login.la \
	$(top_builddir)/daemon/control/libgkd-control.la \
	$(top_builddir)/ui/libgku-prompt.la \
	$(top_builddir)/daemon/ssh-agent/libgkd-ssh-agent.la \
	$(top_builddir)/daemon/gpg-agent/libgkd-gpg-agent.la \
	$(top_builddir)/pkcs11/wrap-layer/libgkm-wrap-layer.la \
	$(top_builddir)/pkcs11/roots-store/libgkm-roots-store.la \
	$(top_builddir)/pkcs11/rpc-layer/libgkm-rpc-layer.la \
	$(top_builddir)/pkcs11/secret-store/libgkm-secret-store.la \
	$(top_builddir)/pkcs11/ssh-store/libgkm-ssh-store.la \
	$(top_builddir)/pkcs11/user-store/libgkm-user-store.la \
	$(top_builddir)/pkcs11/gkm/libgkm.la \
	$(top_builddir)/gck/libgck.la \
	$(top_builddir)/egg/libegg-dbus.la \
	$(DAEMON_LIBS) \
	$(GOBJECT_LIBS) \
	$(GTHREAD_LIBS) \
	$(GIO_LIBS) \
	$(DAEMON_LIBS) \
	$(GOBJECT_LIBS) \
	$(GLIB_LIBS)

service_in_files = \
	org.gnome.keyring.service.in \
	org.freedesktop.secrets.service.in

servicedir       = $(DBUS_SERVICES_DIR)
service_DATA     = $(service_in_files:.service.in=.service)
service_PATH     = $(VALGRIND_RUN)$(bindir)

desktop_in_files = \
	gnome-keyring-pkcs11.desktop.in \
	gnome-keyring-secrets.desktop.in \
	gnome-keyring-ssh.desktop.in \
	gnome-keyring-gpg.desktop.in
desktopdir       = $(sysconfdir)/xdg/autostart
desktop_DATA     = $(desktop_in_files:.desktop.in=.desktop)

EXTRA_DIST = \
	$(service_in_files) \
	$(desktop_in_files)

CLEANFILES = \
	$(service_DATA) \
	$(desktop_DATA)

@INTLTOOL_DESKTOP_RULE@
