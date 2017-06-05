dnl Process this file with autoconf to create configure.
AC_PREREQ([2.53])

AC_INIT([intltool], [0.50.0])
AC_CONFIG_SRCDIR([intltoolize.in])

AM_INIT_AUTOMAKE([foreign])

pkgdatadir='${datadir}'"/${PACKAGE}"
AC_SUBST(pkgdatadir) dnl automake does not need this, but libtoolize does
aclocaldir='${datadir}'/aclocal
AC_SUBST(aclocaldir)

AC_PATH_PROG(PERL, perl)
if test -z "$PERL"; then
   AC_MSG_ERROR([perl not found])
fi
AC_MSG_CHECKING([for perl >= 5.8.1])
$PERL -e "use 5.8.1;" > /dev/null 2>&1
if test $? -ne 0; then
   AC_MSG_ERROR([perl 5.8.1 is required for intltool])
else
   PERL_VERSION="`$PERL -e \"printf '%vd', $^V\"`"
   AC_MSG_RESULT([$PERL_VERSION])
fi
AC_MSG_CHECKING([for XML::Parser])
if `$PERL -e "require XML::Parser" 2>/dev/null`; then
   AC_MSG_RESULT([ok])
else
   AC_MSG_ERROR([XML::Parser perl module is required for intltool])
fi

AC_CONFIG_FILES([
Makefile
doc/Makefile
intltoolize
tests/Makefile
tests/cases/Makefile
tests/results/Makefile
tests/selftest.pl
])

AC_OUTPUT
