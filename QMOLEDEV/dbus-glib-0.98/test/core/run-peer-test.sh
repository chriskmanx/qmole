#!/bin/sh

set -e

# The peer server writes its address over stdout, which the client reads
${DBUS_TOP_BUILDDIR}/libtool --mode=execute ./peer-server | ${DBUS_TOP_BUILDDIR}/libtool --mode=execute ./peer-client
