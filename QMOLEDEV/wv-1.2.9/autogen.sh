#!/bin/sh

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

olddir=`pwd`
cd $srcdir


accheck=`autoconf --version | grep 2.13`
if test "x$accheck" != "x"; then
    echo "error: you appear to be using autoconf 2.13"
    echo "       the automake build system requires autoconf >= 2.50"
    exit 1
fi

aclocal --version > /dev/null 2> /dev/null || {
    echo "error: aclocal not found"
    exit 1
}
automake --version > /dev/null 2> /dev/null || {
    echo "error: automake not found"
    exit 1
}

automake --version | perl -ne 'if (/\(GNU automake\) (([0-9]+).([0-9]+))/) {print; if ($2 < 1 || ($2 == 1 && $3 < 4)) {exit 1;}}'
if [ $? -ne 0 ]; then
    echo "warning: you appear to be using automake <= 1.5"
    echo "         these versions have bugs - GNUmakefile.am dependencies are not generated"
fi

aclocal $ACLOCAL_FLAGS || {
    echo "error: aclocal $ACLOCAL_FLAGS failed"
    exit 1
}

libtoolize --force --copy || {
    echo "error: libtoolize failed"
    exit 1
}

echo "Checking for PKG_CHECK_MODULES..."

pkgcheckdef=`grep PKG_CHECK_MODULES aclocal.m4 | grep AC_DEFUN`
if test "x$pkgcheckdef" = "x"; then
  echo "Running aclocal -I ac-helpers/pkg-config $ACLOCAL_FLAGS"
  (aclocal -I ac-helpers/pkg-config $ACLOCAL_FLAGS 2>> autogen.err) || {
    echo "aclocal failed! Unable to continue."
    exit 1
  }
  pkgcheckdef=`grep PKG_CHECK_MODULES aclocal.m4 | grep AC_DEFUN`
  if test "x$pkgcheckdef" = "x"; then
    echo ""
    echo "error: PKG_CHECK_MODULES isn't defined"
    echo ""
    echo "   Either pkg.m4 wasn't in aclocal's search path or pkgconfig"
    echo "   (or pkgconfig-devel?) isn't installed."
    echo ""
    echo "   If pkg-config is installed in <prefix> then re-run autogen.sh:"
    echo ""
    echo "       ACLOCAL_FLAGS=\"-I <prefix>/share/aclocal\" ./autogen.sh"
    echo ""
    exit
  fi
fi

autoheader || {
    echo "error: autoheader failed"
    exit 1
}
automake -a -c --foreign || {
    echo "warning: automake failed"
}
autoconf || {
    echo "error: autoconf failed"
    exit 1
}

cd $olddir

conf_flags="--enable-maintainer-mode"

if test x$NOCONFIGURE = x; then
  echo Running $srcdir/configure $conf_flags "$@" ...
  $srcdir/configure $conf_flags "$@" \
  && echo Now type \`make\' to compile. || exit 1
else
  echo Skipping configure process.
fi
