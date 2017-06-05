#!/bin/sh

# http://bugs.freedesktop.org/show_bug.cgi?id=19065
echo ${DEBUG} ${top_builddir}/dbus/dbus-binding-tool --mode=glib-server --prefix=test ${srcdir}/data/nested-introspect.xml
${DEBUG} ${top_builddir}/dbus/dbus-binding-tool --mode=glib-server --prefix=test ${srcdir}/data/nested-introspect.xml >/dev/null