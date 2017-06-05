#! /bin/sh

SCRIPTNAME=$0
MODE=$1

## so the tests can complain if you fail to use the script to launch them
DBUS_TEST_GLIB_RUN_TEST_SCRIPT=1
export DBUS_TEST_GLIB_RUN_TEST_SCRIPT
srcdir=`dirname "$0"`
DBUS_TOP_SRCDIR="$srcdir/../.."
export DBUS_TOP_SRCDIR
# Rerun ourselves with tmp session bus if we're not already
if test -z "$DBUS_TEST_GLIB_IN_RUN_TEST"; then
  DBUS_TEST_GLIB_IN_RUN_TEST=1
  export DBUS_TEST_GLIB_IN_RUN_TEST
  exec $DBUS_TOP_SRCDIR/tools/run-with-tmp-session-bus.sh $SCRIPTNAME $MODE
fi  

for x in annotated-node nested-annotation; do
  if ! test -f $srcdir/invalid-$x.xml; then
    echo "invalid-$x.xml missing">&2
    exit 1
  fi

  if $DBUS_BINDING_TOOL --prefix=test_invalid --mode=glib-server \
      --output=invalid-glue.h $srcdir/invalid-$x.xml ||
    $DBUS_BINDING_TOOL --prefix=test_invalid --mode=glib-client \
      --output=invalid-bindings.h $srcdir/invalid-$x.xml; then
    echo "invalid-$x.xml should not have been processed successfully!">&2
    exit 1
  else
    echo "invalid-$x.xml failed, as expected">&2
  fi
done

echo "running test-client"
${DBUS_TOP_BUILDDIR}/libtool --mode=execute $DEBUG $DBUS_TOP_BUILDDIR/test/interfaces/test-client || die "test-client failed"
