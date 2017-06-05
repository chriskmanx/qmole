ACLOCAL_AMFLAGS = -I m4
GLIB_PC=dbus-glib-1.pc

SUBDIRS=dbus tools test doc
DIST_SUBDIRS=dbus tools test doc m4

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = $(GLIB_PC)

DISTCLEANFILES = 		\
	$(GLIB_PC)

EXTRA_DIST =			\
	HACKING			\
	NEWS			\
	dbus-bus-introspect.xml \
	dbus-glib-1.pc.in

# Creating ChangeLog from git log:

MAINTAINERCLEANFILES = ChangeLog

DISTCHECK_CONFIGURE_FLAGS=--enable-gtk-doc --enable-checks --enable-tests
EXTRA_DIST += ChangeLog

ChangeLog:
$(srcdir)/ChangeLog:
	@if test -d "$(srcdir)/.git"; then \
	  (cd "$(srcdir)" && \
	  ./missing --run git log --stat) | fmt --split-only > $@.tmp \
	  && mv -f $@.tmp $@ \
	  || ($(RM) $@.tmp; \
	      echo Failed to generate ChangeLog, your ChangeLog may be outdated >&2; \
	      (test -f $@ || echo git-log is required to generate this file >> $@)); \
	else \
	  test -f $@ || \
	  (echo A git checkout and git log are required to generate ChangeLog >&2 && \
	  echo A git checkout and git-log are required to generate this file >> $@); \
	fi

%.tar.gz.asc: %.tar.gz
	$(AM_V_GEN)gpg --detach-sign --armor $@

maintainer-upload-release:
	test -f @PACKAGE@-@VERSION@.tar.gz
	test -f @PACKAGE@-@VERSION@.tar.gz.asc
	gpg --verify @PACKAGE@-@VERSION@.tar.gz.asc
	rsync -vzP @PACKAGE@-@VERSION@.tar.gz dbus.freedesktop.org:/srv/dbus.freedesktop.org/www/releases/@PACKAGE@/@PACKAGE@-@VERSION@.tar.gz
	rsync -vzP @PACKAGE@-@VERSION@.tar.gz.asc dbus.freedesktop.org:/srv/dbus.freedesktop.org/www/releases/@PACKAGE@/@PACKAGE@-@VERSION@.tar.gz.asc
	rsync -rvzPp --chmod=Dg+s,ug+rwX,o=rX $(srcdir)/docs/reference/html/ \
	dbus.freedesktop.org:/srv/dbus.freedesktop.org/www/doc/@PACKAGE@/

include tools/lcov.am
