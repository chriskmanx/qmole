AC_INIT(daemon/gkd-main.c)

AM_INIT_AUTOMAKE(gnome-keyring, 3.1.4)
AM_CONFIG_HEADER(config.h)
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

dnl ****************************************************************************
dnl Library package and libtool versioning
dnl
dnl Updating the libtool versions, follow these instructions sequentially:
dnl  1. If the library source code has changed at all since the last update, then increment revision (‘c:r:a’ becomes ‘c:r+1:a’).
dnl  2. If any interfaces have been added, removed, or changed since the last update, increment current, and set revision to 0.
dnl  3. If any interfaces have been added since the last public release, then increment age.
dnl  4. If any interfaces have been removed or changed since the last public release, then set age to 0.

# GCK package versioning
GCK_MAJOR=1
GCK_MINOR=0
GCK_MICRO=0

# GCK libtool versioning
GCK_CURRENT=0
GCK_REVISION=0
GCK_AGE=0

# GCR package versioning
GCR_MAJOR=3
GCR_MINOR=0
GCR_MICRO=0

# GCR libtool versioning
GCR_CURRENT=0
GCR_REVISION=0
GCR_AGE=0

dnl ****************************************************************************

version_bits=`echo $VERSION | tr '.' ' '`
full_version=`printf %03d%03d%03d $version_bits`
AC_DEFINE_UNQUOTED(GKR_VERSION, $full_version, [The gnome-keyring version as a number])

AM_SANITY_CHECK
AM_MAINTAINER_MODE

AC_GNU_SOURCE
AC_ISC_POSIX
AC_PROG_CC
AC_PROG_CPP
AM_PROG_CC_C_O
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AM_DISABLE_STATIC
AM_PROG_LIBTOOL
IT_PROG_INTLTOOL([0.35.0])
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)

AC_TYPE_PID_T
AC_TYPE_SIGNAL
AC_TYPE_SIZE_T
AC_TYPE_UID_T

GTK_DOC_CHECK(1.9)
DISTCHECK_CONFIGURE_FLAGS="--enable-gtk-doc"
AC_SUBST(DISTCHECK_CONFIGURE_FLAGS)

PKG_CHECK_MODULES(GLIB, glib-2.0 >= 2.16.0)
AC_SUBST(GLIB_CFLAGS)
AC_SUBST(GLIB_LIBS)

PKG_CHECK_MODULES(GMODULE, gmodule-no-export-2.0)
AC_SUBST(GLIB_CFLAGS)
AC_SUBST(GLIB_LIBS)

PKG_CHECK_MODULES(GTHREAD, gthread-2.0 >= 2.8.0)
AC_SUBST(GTHREAD_CFLAGS)
AC_SUBST(GTHREAD_LIBS)

PKG_CHECK_MODULES(GOBJECT, glib-2.0 >= 2.16.0 gobject-2.0 >= 2.8.0)
AC_SUBST(GOBJECT_CFLAGS)
AC_SUBST(GOBJECT_LIBS)

PKG_CHECK_MODULES(GIO, glib-2.0 >= 2.25.0 gio-2.0)
AC_SUBST(GIO_CFLAGS)
AC_SUBST(GIO_LIBS)

PKG_CHECK_MODULES(GTK, gtk+-3.0 >= 3.0)
AC_SUBST(GTK_CFLAGS)
AC_SUBST(GTK_LIBS)

GETTEXT_PACKAGE=gnome-keyring
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE,"$GETTEXT_PACKAGE", [The gettext domain name])
AC_SUBST(GETTEXT_PACKAGE)

AM_GLIB_GNU_GETTEXT

AC_PATH_PROG(GLIB_GENMARSHAL, glib-genmarshal)
AC_PATH_PROG(GLIB_MKENUMS, glib-mkenums)

GLIB_GSETTINGS

# --------------------------------------------------------------------
# Check for socklen_t
#

AC_MSG_CHECKING(whether socklen_t is defined)
AC_TRY_COMPILE([
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
],[
socklen_t foo;
foo = 1;
foo = foo;
],have_socklen_t=yes,have_socklen_t=no)
AC_MSG_RESULT($have_socklen_t)

if test "x$have_socklen_t" = "xyes"; then
    AC_DEFINE(HAVE_SOCKLEN_T,1,[Have socklen_t type])
fi

# --------------------------------------------------------------------
# Check for various credentials.
#

AC_MSG_CHECKING(for struct cmsgcred)
AC_TRY_COMPILE([
#include <sys/types.h>
#include <sys/socket.h>
],[
struct cmsgcred cred;

cred.cmcred_pid = 0;
],have_struct_cmsgcred=yes,have_struct_cmsgcred=no)
AC_MSG_RESULT($have_struct_cmsgcred)

if test x$have_struct_cmsgcred = xyes; then
    AC_DEFINE(HAVE_CMSGCRED,1,[Have cmsgcred structure])
fi

# --------------------------------------------------------------------
# Check for getpeerucred
#

AC_CHECK_FUNCS(getpeerucred, AC_DEFINE(HAVE_GETPEERUCRED,1,[Have getpeerucred]))

# --------------------------------------------------------------------
# Check for getpeereid
#

AC_CHECK_FUNCS(getpeereid, AC_DEFINE(HAVE_GETPEEREID, 1, [Have getpeereid function]))

# --------------------------------------------------------------------
# Check for flock
#

AC_CHECK_FUNCS(flock, AC_DEFINE(HAVE_FLOCK,1,[Have flock]))

# --------------------------------------------------------------------
# Check for timegm
#

AC_CHECK_FUNCS(timegm, AC_DEFINE(HAVE_TIMEGM,1,[Have timegm]))

dnl mkdtemp replacement from gettext
AC_REPLACE_FUNCS(mkdtemp)
AC_STAT_MACROS_BROKEN
  AC_CACHE_CHECK([for inttypes.h], jm_ac_cv_header_inttypes_h,
  [AC_TRY_COMPILE(
    [#include <sys/types.h>
#include <inttypes.h>],
    [uintmax_t i = (uintmax_t) -1;],
    jm_ac_cv_header_inttypes_h=yes,
    jm_ac_cv_header_inttypes_h=no)])
  if test $jm_ac_cv_header_inttypes_h = yes; then
    AC_DEFINE_UNQUOTED(HAVE_INTTYPES_H_WITH_UINTMAX, 1,
[Define if <inttypes.h> exists, doesn't clash with <sys/types.h>,
   and declares uintmax_t. ])
  fi
  AC_CACHE_CHECK([for stdint.h], jm_ac_cv_header_stdint_h,
  [AC_TRY_COMPILE(
    [#include <sys/types.h>
#include <stdint.h>],
    [uintmax_t i = (uintmax_t) -1;],
    jm_ac_cv_header_stdint_h=yes,
    jm_ac_cv_header_stdint_h=no)])
  if test $jm_ac_cv_header_stdint_h = yes; then
    AC_DEFINE_UNQUOTED(HAVE_STDINT_H_WITH_UINTMAX, 1,
[Define if <stdint.h> exists, doesn't clash with <sys/types.h>,
   and declares uintmax_t. ])
  fi
AC_CHECK_HEADERS(fcntl.h sys/time.h time.h unistd.h)
AC_CHECK_FUNCS(gettimeofday fsync)

# --------------------------------------------------------------------
# Memory locking
#

AC_CHECK_FUNCS(mlock)

# --------------------------------------------------------------------
# socket()
#

AC_CHECK_LIB(socket, socket)

have_socket=no
AC_CHECK_FUNCS(socket, have_socket=yes)
if test $have_socket = no; then
  # socket is not in the default libraries.  See if it's in some other.
  for lib in bsd socket inet; do
    AC_CHECK_LIB($lib, socket, [
	  DAEMON_LIBS="$DAEMON_LIBS -l$lib"
	  have_socket=yes
	  AC_DEFINE(HAVE_SOCKET)
	  break])
  done
fi

# --------------------------------------------------------------------
# DBus Checks
#

DBUS_REQUIRED=1.0
PKG_CHECK_MODULES([DBUS], dbus-1 >= 1.0)

DBUS_VERSION=`$PKG_CONFIG --modversion dbus-1`
DBUS_MAJOR_VERSION=`echo $DBUS_VERSION | cut -d. -f1`
DBUS_MINOR_VERSION=`echo $DBUS_VERSION | cut -d. -f2`
DBUS_MICRO_VERSION=`echo $DBUS_VERSION | cut -d. -f3`
DBUS_CFLAGS="$DBUS_CFLAGS -DGKR_DBUS_MAJOR_VERSION=$DBUS_MAJOR_VERSION"
DBUS_CFLAGS="$DBUS_CFLAGS -DGKR_DBUS_MINOR_VERSION=$DBUS_MINOR_VERSION"
DBUS_CFLAGS="$DBUS_CFLAGS -DGKR_DBUS_MICRO_VERSION=$DBUS_MICRO_VERSION"

DAEMON_CFLAGS="$DAEMON_CFLAGS $DBUS_CFLAGS"
DAEMON_LIBS="$DAEMON_LIBS $DBUS_LIBS"

AC_ARG_WITH(dbus-services,
		  [AC_HELP_STRING([--with-dbus-services=<dir>],
		  [where D-BUS session services directory is])])
if ! test -z "$with_dbus_services" ; then
	DBUS_SERVICES_DIR="$with_dbus_services"
else
	DBUS_SERVICES_DIR="$datadir/dbus-1/services"
fi

AC_SUBST(DBUS_SERVICES_DIR)

# --------------------------------------------------------------------
# PAM Checks

AC_ARG_ENABLE(pam,
	    AC_HELP_STRING([--disable-pam],
	    [Build GNOME keyring PAM helper]))

pam_status="no"
if test "$enable_pam" != "no"; then
	AC_CHECK_HEADERS(security/pam_modules.h pam/pam_modules.h, [have_pam=yes; break], have_pam=no)
	if test "$enable_pam" = "yes" -a "$have_pam" = "no"; then
		AC_MSG_ERROR(The PAM headers are missing)
	elif test "$have_pam" = "yes"; then
		old_LIBS=$LIBS
		AC_CHECK_LIB(pam, pam_start, have_pam=yes, have_pam=no)
		if test "$enable_pam" = "yes" -a "$have_pam" = "no"; then
			AC_MSG_ERROR(The PAM library is missing)
		fi
		LIBS=$old_LIBS
	fi
fi

# --------------------------------------------------------------------
# Find DL functionality

AC_CHECK_LIB(c, dlopen, DL_LIBS="",
	AC_CHECK_LIB(dl, dlopen, DL_LIBS="-ldl",
		AC_MSG_ERROR([Could not find a library with the dlopen function])
	)
)

AC_SUBST(DL_LIBS)

# --------------------------------------------------------------------
# p11-kit

PKG_CHECK_MODULES([P11_KIT], p11-kit-1)
AC_SUBST(P11_KIT_CFLAGS)
AC_SUBST(P11_KIT_LIBS)

AC_MSG_CHECKING([where to put pkcs11 module configuration])
P11_SYSTEM_MODULES=`$PKG_CONFIG p11-kit-1 --variable=p11_system_modules`
AC_MSG_RESULT($P11_SYSTEM_MODULES)
AC_SUBST(P11_SYSTEM_MODULES)

if "$P11_SYSTEM_MODULES" = ""; then
	AC_MSG_ERROR([Could not find location for pkcs11 module config])
fi

# --------------------------------------------------------------------
# PAM Module Directory

AC_ARG_WITH([pam-dir],
            [AC_HELP_STRING([--with-pam-dir=DIR],
                             [directory to install pam modules in])],
             [], [with_pam_dir='${libdir}/security'])
PAM_DEST_DIR="$with_pam_dir"
AC_SUBST(PAM_DEST_DIR)

AM_CONDITIONAL(WITH_PAM, test "$have_pam" = "yes")

if test "$have_pam" = "yes"; then
	pam_status="yes ($with_pam_dir)"
fi

# --------------------------------------------------------------------
# SSH Agent support
#

AC_ARG_ENABLE([ssh-agent],
		AC_HELP_STRING([--disable-ssh-agent],
		[Don't include SSH agent in gnome-keyring]))

if test "$enable_ssh_agent" != "no"; then
	AC_DEFINE(WITH_SSH, 1, [Whether to build SSH agent or not])
	ssh_status="yes"
else
	ssh_status="no"
fi

AM_CONDITIONAL(WITH_SSH, test "$enable_ssh_agent" != "no")

# --------------------------------------------------------------------
# GPG Agent support
#

AC_ARG_ENABLE([gpg-agent],
		AC_HELP_STRING([--disable-gpg-agent],
		[Don't include GPG agent in gnome-keyring]))

if test "$enable_gpg_agent" != "no"; then
	AC_DEFINE(WITH_GPG, 1, [Whether to build GPG agent or not])
	gpg_status="yes"
else
	gpg_status="no"
fi

AM_CONDITIONAL(WITH_GPG, test "$enable_gpg_agent" != "no")

# --------------------------------------------------------------------
# GPG support

AC_PATH_PROGS([GNUPG], [gpg gpg2], ["gpg"])
AC_DEFINE_UNQUOTED([GPG_EXECUTABLE], ["$GNUPG"], [Path to gpg executable.])

# --------------------------------------------------------------------
# Trusted Root Certificates Directory
#

AC_MSG_CHECKING([location of system Certificate Authority list])
AC_ARG_WITH(root-certs,
		[AC_HELP_STRING([--with-root-certs=@<:@path@:>@],
			[path to system Certificate Authority list])])
if test "$with_root_certs" = "no"; then
	AC_MSG_RESULT([disabled])
	root_status="none"
	with_root_certs="no"
else
	if test -z "$with_root_certs"; then
		for f in /etc/pki/tls/certs /etc/ssl/certs; do
			if test -d "$f"; then
				with_root_certs="$f"
			fi
		done
		if test -z "$with_root_certs"; then
			AC_MSG_ERROR([could not find. Use --with-root-certs=path to set, or --without-root-certs to disable])
		fi
	fi

	AC_MSG_RESULT($with_root_certs)
	if ! test -d "$with_root_certs"; then
		AC_MSG_ERROR([No such directory '$with_root_certs'. Use --with-root-certs=path to set, or --without-root-certs to disable])
	fi

	root_status="yes ($with_root_certs)"
	AC_DEFINE_UNQUOTED([ROOT_CERTIFICATES], ["$with_root_certs"], [path to system Certificate Authority list])
fi

# --------------------------------------------------------------------
# Check for /dev/random
#

AC_CHECK_FILE(/dev/random, AC_DEFINE(HAVE_DEVRANDOM,1,[Have /dev/random]))


# --------------------------------------------------------------------
# libgcrypt
#

GCRYPT_VERSION=1.2.2
GCRYPT_LIBVER=1

AM_PATH_LIBGCRYPT($GCRYPT_LIBVER:$GCRYPT_VERSION,,
	AC_MSG_ERROR([[
***
*** libgcrypt was not found. You may want to get it from
*** ftp://ftp.gnupg.org/gcrypt/libgcrypt/
***
]]))
libgcrypt=yes
AC_DEFINE_UNQUOTED(LIBGCRYPT_VERSION, "$GCRYPT_VERSION", [Version of GCRYPT we expect])

DAEMON_CFLAGS="$DAEMON_CFLAGS $LIBGCRYPT_CFLAGS"
DAEMON_LIBS="$DAEMON_LIBS $LIBGCRYPT_LIBS"

AC_SUBST([LIBGCRYPT_CFLAGS])
AC_SUBST([LIBGCRYPT_LIBS])

# -------------------------------------------------------------------
# libtasn1
#

GKR_LIBTASN1_VERSION=0.3.4

PKG_CHECK_MODULES(LIBTASN1,libtasn1 >= $GKR_LIBTASN1_VERSION)
AC_SUBST([LIBTASN1_LIBS])
AC_SUBST([LIBTASN1_CFLAGS])

DAEMON_CFLAGS="$DAEMON_CFLAGS $LIBTASN1_CFLAGS"
DAEMON_LIBS="$DAEMON_LIBS"

AC_PATH_PROG(ASN1PARSER, asn1Parser, no)
if test "$ASN1PARSER" = "no" ; then
	AC_MSG_ERROR([asn1Parser tool is not installed, try libtasn1 or a related package])
fi

# -------------------------------------------------------------------
# libcap-ng
#

AC_CHECK_LIB([cap-ng], [capng_clear], have_libcapng="yes", have_libcapng="no")

if test "$have_libcapng" = "yes"; then
   AC_DEFINE(HAVE_LIBCAPNG, 1, [Have libcap-ng package, libcap-ng library])
   DAEMON_LIBS="$DAEMON_LIBS -lcap-ng"
else
   have_lipcapng="no"
   AC_MSG_WARN([libcap-ng (or development headers) is not installed])
fi

AM_CONDITIONAL(WITH_CAPS, test "$have_libcapng" = "yes")
libcapng_status=$have_libcapng

# --------------------------------------------------------------------
# Debug mode
#

AC_MSG_CHECKING([for debug mode])
AC_ARG_ENABLE(debug,
              AC_HELP_STRING([--enable-debug=no/yes/full],
              [Turn on or off debugging]))

if test "$enable_debug" != "no"; then
	AC_DEFINE_UNQUOTED(WITH_DEBUG, 1, [Print debug output])
	AC_DEFINE_UNQUOTED(_DEBUG, 1, [In debug mode])
fi

if test "$enable_debug" = "full"; then
	debug_status="full"
	CFLAGS="$CFLAGS -g -O0 -Werror"
	CFLAGS="$CFLAGS -DG_DISABLE_DEPRECATED -DGDK_PIXBUF_DISABLE_DEPRECATED"
	CFLAGS="$CFLAGS -DGDK_DISABLE_DEPRECATED -DGTK_DISABLE_DEPRECATED"
elif test "$enable_debug" = "no"; then
	debug_status="no"
	AC_DEFINE_UNQUOTED(G_DISABLE_ASSERT, 1, [Disable glib assertions])
else
	debug_status="yes"
fi

AC_MSG_RESULT($debug_status)

# -------------------------------------------------------------------
# More warnings

AC_MSG_CHECKING(for more warnings)
if test "$GCC" = "yes"; then
	AC_MSG_RESULT(yes)
	CFLAGS="\
	-Wall \
	-Wchar-subscripts -Wmissing-declarations -Wmissing-prototypes \
	-Wnested-externs -Wpointer-arith \
	-Wcast-align -Wsign-compare \
	$CFLAGS"

	# Disable -Werror for now, because of issues with
	# strange gtk-doc programs not building cleanly.
	# -Werror \

	for option in -Wno-strict-aliasing -Wno-sign-compare; do
		SAVE_CFLAGS="$CFLAGS"
		CFLAGS="$CFLAGS $option"
		AC_MSG_CHECKING([whether gcc understands $option])
		AC_TRY_COMPILE([], [],
			has_option=yes,
			has_option=no,)
		if test $has_option = no; then
			CFLAGS="$SAVE_CFLAGS"
		fi
		AC_MSG_RESULT($has_option)
		unset has_option
		unset SAVE_CFLAGS
	done
	unset option
else
	AC_MSG_RESULT(no)
fi

# --------------------------------------------------------------------
# Tests and Unit Tests

AC_ARG_ENABLE(tests,
	    AC_HELP_STRING([--enable-tests=yes/no/full],
	    [Build tests and testing tools. default: yes]))

AC_MSG_CHECKING([build test tools, unit tests, and -Werror])

if test "$enable_tests" = "full"; then
	tests_status="full"
	CFLAGS="$CFLAGS -Werror"
	TEST_MODE="thorough"
	AC_DEFINE_UNQUOTED(WITH_TESTABLE, 1, [Build extra hooks for more testable code])
elif test "$enable_tests" = "no"; then
	tests_status="no"
else
	TEST_MODE="quick"
	tests_status="yes"
fi

AC_MSG_RESULT($tests_status)
AC_SUBST(TEST_MODE)
AM_CONDITIONAL(WITH_TESTS, test "$enable_tests" != "no")

# ----------------------------------------------------------------------
# Coverage

AC_MSG_CHECKING([whether to build with gcov testing])
AC_ARG_ENABLE([gcov],
		AS_HELP_STRING([--enable-gcov],
			[Whether to enable coverage testing ]),
		[],
		[enable_gcov=no])

AC_MSG_RESULT([$enable_gcov])

if test "$enable_gcov" = "yes"; then
	if test "$GCC" != "yes"; then
		AC_MSG_ERROR(Coverage testing requires GCC)
	fi

	AC_PATH_PROG(GCOV, gcov, no)
	if test "$GCOV" = "no" ; then
		AC_MSG_ERROR(gcov tool is not available)
	fi

	AC_PATH_PROG(LCOV, lcov, no)
	if test "$LCOV" = "no" ; then
		AC_MSG_ERROR(lcov tool is not installed)
	fi

	AC_PATH_PROG(GENHTML, genhtml, no)
	if test "$GENHTML" = "no" ; then
		AC_MSG_ERROR(lcov's genhtml tool is not installed)
	fi

	CFLAGS="$CFLAGS -O0 -g -fprofile-arcs -ftest-coverage"
	LDFLAGS="$LDFLAGS -lgcov"
fi

AM_CONDITIONAL([WITH_COVERAGE], [test "$enable_gcov" = "yes"])
AC_SUBST(LCOV)
AC_SUBST(GCOV)
AC_SUBST(GENHTML)

# ----------------------------------------------------------------------
# selinux

LIBSELINUX=""
selinux_status="no"
AC_ARG_ENABLE([selinux],
        AC_HELP_STRING([--disable-selinux],[do not use SELinux]))
if test "x$enable_selinux" != "xno"; then
    AC_CHECK_LIB([selinux],[getfilecon],
        [AC_CHECK_LIB([selinux],[setexeccon],
            [AC_DEFINE([WITH_SELINUX], 1, [Defined if SE Linux support is compiled in])
             LIBSELINUX="-lselinux"
             selinux_status="yes"])
        ])
fi
AC_SUBST(LIBSELINUX)
AM_CONDITIONAL([HAVE_LIBSELINUX], [test ! -z "$LIBSELINUX"])

# ----------------------------------------------------------------------
# Valgrind

AC_ARG_ENABLE(valgrind,
	AC_HELP_STRING([--enable-valgrind],
	[Run gnome-keyring-daemon using valgrind]))

AC_MSG_CHECKING([valgrind])
valgrind_status="yes"

AM_CONDITIONAL(WITH_VALGRIND, test "$enable_valgrind" = "run")

# Run gnome-keyring-daemon under valgrind as default
if test "$enable_valgrind" = "run"; then
	AC_PATH_PROG(VALGRIND, valgrind, no)
	if test "$VALGRIND" = "no" ; then
		AC_MSG_ERROR(valgrind tool is not installed)
	fi

	VALGRIND_ARG="--log-file=/tmp/gkr-valgrind.log.%p"
	VALGRIND_RUN="$VALGRIND $VALGRIND_ARG "

	AC_DEFINE_UNQUOTED(VALGRIND, "$VALGRIND", [Path to valgrind executable])
	AC_DEFINE_UNQUOTED(VALGRIND_ARG, "$VALGRIND_ARG", [Valgrind arguments])

	enable_valgrind="yes"
	valgrind_status="run"
fi

AC_SUBST(VALGRIND)
AC_SUBST(VALGRIND_ARG)
AC_SUBST(VALGRIND_RUN)

# Build valgrind support into code
if test "$enable_valgrind" = "yes"; then
	AC_CHECK_HEADER([valgrind/valgrind.h], have_valgrind=yes, have_valgrind=no)
	if test "$have_valgrind" = "no"; then
		AC_MSG_ERROR(The valgrind headers are missing)
	fi
	AC_DEFINE_UNQUOTED(WITH_VALGRIND, 1, [Run under valgrind])
	AC_MSG_RESULT(yes)


# No valgrind
else
	AC_MSG_RESULT(no)
	valgrind_status="no"
fi

# ----------------------------------------------------------------------
# p11-tests

AC_ARG_ENABLE(p11_tests,
	AC_HELP_STRING([--disable-p11-tests],
	[Build in p11-tests for testing PKCS#11 modules]))

p11_tests_status="no"

if test "$enable_p11_tests" != "no"; then
	PKG_CHECK_MODULES(P11_TESTS, p11-tests >= 0.1,
	                  p11_tests_status=yes, p11_tests_status=no)
fi

AM_CONDITIONAL(WITH_P11_TESTS, test "$p11_tests_status" = "yes")

if test "$p11_tests_status" = "yes"; then
	AC_DEFINE_UNQUOTED(WITH_P11_TESTS, 1, [Run p11-tests module testing])
fi

# ----------------------------------------------------------------------

GCK_LT_RELEASE=$GCK_CURRENT:$GCK_REVISION:$GCK_AGE
AC_SUBST(GCK_LT_RELEASE)
AC_SUBST(GCK_MAJOR)

GCR_LT_RELEASE=$GCR_CURRENT:$GCR_REVISION:$GCR_AGE
AC_SUBST(GCR_LT_RELEASE)
AC_SUBST(GCR_MAJOR)

AC_SUBST(DAEMON_CFLAGS)
AC_SUBST(DAEMON_LIBS)

AC_SUBST(P11_TESTS_CFLAGS)
AC_SUBST(P11_TESTS_LIBS)

# Are we running with GNU Make?
AM_CONDITIONAL([WITH_GNU_MAKE], [make --version | grep -q 'GNU Make' > /dev/null])

# HACK: Because autoconf is nasty
if test "${prefix}" = "NONE"; then
    prefix=/usr/local
fi
if test "${exec_prefix}" = "NONE"; then
    exec_prefix=$prefix
fi

eval BINDIR="$bindir"
AC_SUBST(BINDIR)

eval LIBEXECDIR="$libexecdir"
AC_SUBST(LIBEXECDIR)

eval pkcs11dir="${libdir}/pkcs11"
AC_SUBST(pkcs11dir)

pkcs11standalonedir="${libdir}/gnome-keyring/devel"
AC_SUBST(pkcs11standalonedir)

AC_OUTPUT([
Makefile
daemon/Makefile
daemon/gnome-keyring-gpg.desktop.in
daemon/gnome-keyring-pkcs11.desktop.in
daemon/gnome-keyring-secrets.desktop.in
daemon/gnome-keyring-ssh.desktop.in
daemon/org.gnome.keyring.service
daemon/org.freedesktop.secrets.service
daemon/control/Makefile
daemon/control/tests/Makefile
daemon/dbus/Makefile
daemon/dbus/tests/Makefile
daemon/gpg-agent/Makefile
daemon/login/Makefile
daemon/ssh-agent/Makefile
docs/Makefile
docs/reference/Makefile
docs/reference/gck/Makefile
docs/reference/gck/version.xml
docs/reference/gcr/Makefile
docs/reference/gcr/version.xml
egg/Makefile
egg/tests/Makefile
gck/gck.pc
gck/Makefile
gck/tests/Makefile
gcr/gcr.pc
gcr/Makefile
gcr/icons/Makefile
gcr/icons/16x16/Makefile
gcr/icons/22x22/Makefile
gcr/icons/24x24/Makefile
gcr/icons/32x32/Makefile
gcr/icons/48x48/Makefile
gcr/tests/Makefile
pam/Makefile
pkcs11/Makefile
pkcs11/gkm/Makefile
pkcs11/gkm/tests/Makefile
pkcs11/gnome2-store/Makefile
pkcs11/gnome2-store/tests/Makefile
pkcs11/roots-store/Makefile
pkcs11/roots-store/tests/Makefile
pkcs11/rpc-layer/Makefile
pkcs11/rpc-layer/gnome-keyring-module
pkcs11/secret-store/Makefile
pkcs11/secret-store/tests/Makefile
pkcs11/ssh-store/Makefile
pkcs11/ssh-store/tests/Makefile
pkcs11/wrap-layer/Makefile
pkcs11/wrap-layer/tests/Makefile
pkcs11/xdg-store/Makefile
pkcs11/xdg-store/tests/Makefile
po/Makefile.in
schema/Makefile
testing/Makefile
tool/Makefile
ui/Makefile
ui/gnome-keyring-prompt.desktop.in
ui/tests/Makefile
])

# ------------------------------------------------------------------------------
# Summary
#

echo
echo "OPTIONAL DEPENDENCIES"
echo "  PAM:                  $pam_status"
echo "  Linux capabilities:   $libcapng_status"
echo "  SELinux:              $selinux_status"
echo
echo "CONFIGURATION"
echo "  SSH Agent:            $ssh_status"
echo "  GPG Agent:            $gpg_status"
echo "  Root Certificates:    $root_status"
echo "  PKCS#11 Modules:      $P11_SYSTEM_MODULES"
echo
echo "BUILD"
echo "  Debug Build:          $debug_status"
echo "  Valgrind:             $valgrind_status"
echo "  PKCS#11 Tests         $p11_tests_status"
echo "  Tests, -Werror:       $tests_status"
echo "  Test Coverage:        $enable_gcov"
echo
