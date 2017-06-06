libexec_PROGRAMS = gconf-defaults-mechanism

gconf-defaults-glue.h: $(srcdir)/gconf-defaults.xml
	dbus-binding-tool --prefix=gconf_defaults --mode=glib-server      \
			  --output=gconf-defaults-glue.h                  \
			  $(srcdir)/gconf-defaults.xml


gconf_defaults_mechanism_SOURCES = \
	gconf-defaults.h \
	gconf-defaults.c \
	gconf-defaults-glue.h \
	gconf-defaults-main.c

INCLUDES = \
	-I$(top_srcdir) \
	-I$(top_builddir) \
	-DSYSGCONFDIR=\"$(sysgconfdir)\" \
	$(DEFAULTS_CFLAGS)

gconf_defaults_mechanism_LDADD = \
	$(top_builddir)/gconf/libgconf-2.la \
	$(DEFAULTS_LIBS)

BUILT_SOURCES = gconf-defaults-glue.h

dbus_servicesdir = $(datadir)/dbus-1/system-services
dbus_confdir = $(sysconfdir)/dbus-1/system.d
polkitdir = $(datadir)/polkit-1/actions

dbus_services_in_files = org.gnome.GConf.Defaults.service.in
polkit_in_files = org.gnome.gconf.defaults.policy.in

dbus_services_DATA = $(dbus_services_in_files:.service.in=.service)

$(dbus_services_DATA): $(dbus_services_in_files)
	sed -e "s|\@LIBEXECDIR\@|$(libexecdir)|" $< > $@

dbus_conf_DATA = org.gnome.GConf.Defaults.conf

@INTLTOOL_POLICY_RULE@
polkit_DATA = $(polkit_in_files:.policy.in=.policy)

EXTRA_DIST =					\
	$(dbus_services_in_files)		\
	org.gnome.GConf.Defaults.conf		\
	$(polkit_in_files)			\
	gconf-defaults.xml

CLEANFILES =			\
	$(BUILT_SOURCES)	\
	$(polkit_DATA)		\
	$(dbus_services_DATA)

DISTCLEANFILES = org.gnome.gconf.defaults.policy

