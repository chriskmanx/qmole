
AC_PREREQ(2.59)
AC_INIT(src/main.c)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR([m4])

PACKAGE=claws-mail

dnl version number
MAJOR_VERSION=3
MINOR_VERSION=9
MICRO_VERSION=0
INTERFACE_AGE=0
BINARY_AGE=0
EXTRA_VERSION=0
EXTRA_RELEASE=
EXTRA_GTK2_VERSION=

if test \( $EXTRA_VERSION -eq 0 \) -o \( "x$EXTRA_RELEASE" != "x" \); then
    VERSION=${MAJOR_VERSION}.${MINOR_VERSION}.${MICRO_VERSION}${EXTRA_RELEASE}${EXTRA_GTK2_VERSION}
else
    VERSION=${MAJOR_VERSION}.${MINOR_VERSION}.${MICRO_VERSION}cvs${EXTRA_VERSION}${EXTRA_GTK2_VERSION}
fi

dnl set $target
AC_CANONICAL_SYSTEM

dnl
AM_INIT_AUTOMAKE($PACKAGE, $VERSION, no-define)
dnl AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE")
dnl AC_DEFINE_UNQUOTED(VERSION, "$VERSION")
AC_SUBST(PACKAGE)
AC_SUBST(VERSION)
AC_SUBST(MAJOR_VERSION)
AC_SUBST(MINOR_VERSION)
AC_SUBST(MICRO_VERSION)
AC_SUBST(EXTRA_VERSION)

dnl GNOME installed?
AC_PATH_PROG(GNOME_CONFIG, gnome-config, no)
if test "$GNOME_CONFIG" != no; then
	gnomedatadir="`gnome-config --datadir`"
	gnomeprefix="`gnome-config --prefix`"
	if test "${prefix}" = "NONE"; then
		gnomedatadir="${ac_default_prefix}/${gnomedatadir#${gnomeprefix}}"
	else
		gnomedatadir="${prefix}/${gnomedatadir#${gnomeprefix}}"
	fi
	AC_SUBST(gnomedatadir)
fi
AM_CONDITIONAL(CLAWS_GNOME, test -n "$gnomedatadir")

AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test x$PKG_CONFIG = xno ; then
  AC_MSG_ERROR([*** pkg-config not found. See http://www.freedesktop.org/software/pkgconfig/])
fi

dnl GNOME 2.x installed?
PKG_CHECK_MODULES(GNOME2, libgnome-2.0 >= 2.0, ac_enable_gnome2=yes, ac_enable_gnome2=no)
AM_CONDITIONAL(CLAWS_GNOME2, test x"$ac_enable_gnome2" = x"yes")

dnl libtool versioning
LT_RELEASE=$MAJOR_VERSION.$MINOR_VERSION
LT_CURRENT=`expr $MICRO_VERSION - $INTERFACE_AGE`
LT_REVISION=$INTERFACE_AGE
LT_AGE=`expr $BINARY_AGE - $INTERFACE_AGE`
AC_SUBST(LT_RELEASE)
AC_SUBST(LT_CURRENT)
AC_SUBST(LT_REVISION)
AC_SUBST(LT_AGE)

dnl Specify a header configuration file
AC_CONFIG_HEADERS(config.h)
AC_CONFIG_HEADERS(claws-features.h)

AM_MAINTAINER_MODE

dnl Checks for programs.
dnl AC_ARG_PROGRAM
AC_PROG_CC
AC_ISC_POSIX
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_CPP
dnl AC_PROG_RANLIB
AM_PROG_LEX
AC_PROG_YACC
AC_LIB_PREFIX
AC_LIBTOOL_WIN32_DLL
LT_INIT
LT_AC_PROG_RC
AC_LIBTOOL_RC
AC_PROG_LIBTOOL

AC_SYS_LARGEFILE

dnl ******************************
dnl Checks for host
dnl Not needed anymore because we 
dnl do AC_CANONICAL_SYSTEM above
dnl ******************************
dnl AC_CANONICAL_HOST

dnl Copied from the official gtk+-2 configure.in
AC_MSG_CHECKING([for some Win32 platform])
case "$host" in
  *-*-mingw*|*-*-cygwin*)
    platform_win32=yes
    LDFLAGS="$LDFLAGS -mwindows"
    ;;
  *)
    platform_win32=no
    ;;
esac
AC_MSG_RESULT([$platform_win32])
AM_CONDITIONAL(PLATFORM_WIN32, test x"$platform_win32" = x"yes")

AC_MSG_CHECKING([for native Win32])
case "$host" in
  *-*-mingw*)
    os_win32=yes
    ;;
  *)
    os_win32=no
    ;;
esac
AC_MSG_RESULT([$os_win32])
AM_CONDITIONAL(OS_WIN32, test x"$os_win32" = x"yes")

AC_MSG_CHECKING([for Cygwin])
case "$host" in
  *-*-cygwin*)
    env_cygwin=yes
    ;;
  *)
    env_cygwin=no
    ;;
esac
AC_MSG_RESULT([$env_cygwin])
AM_CONDITIONAL(CYGWIN, test x"$env_cygwin" = x"yes")

if test "$GCC" = "yes"
then
	CFLAGS="$CFLAGS -Wno-unused-function"
fi

AC_MSG_CHECKING([if gcc supports -Wno-pointer-sign])
_gcc_cflags_save=$CFLAGS
CFLAGS="-Wno-pointer-sign"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],_gcc_psign=yes,_gcc_psign=no)
AC_MSG_RESULT($_gcc_psign)
CFLAGS=$_gcc_cflags_save;
if test x"$_gcc_psign" = xyes ; then
	CFLAGS="$CFLAGS -Wno-pointer-sign"
fi

CFLAGS="$CFLAGS -Wall"

#if test $USE_MAINTAINER_MODE = yes; then
#	CFLAGS="-g -Wall -Wno-pointer-sign -DGTK_DISABLE_DEPRECATED -DGDK_DISABLE_DEPRECATED -DG_DISABLE_DEPRECATED -DGTK_DISABLE_SINGLE_INCLUDES -DGDK_PIXBUF_DISABLE_DEPRECATED -DGSEAL_ENABLE"
#fi

pthread_name=
case "$target" in
*-darwin*)
	CFLAGS="$CFLAGS -no-cpp-precomp -fno-common"
	;;
*-*-mingw*)
        # Note that we need to link to pthreadGC2 in all cases. This
        # is because some locking is used even when pthread support is
        # disabled.
        pthread_name=pthreadGC2
	CFLAGS="$CFLAGS -mms-bitfields"
	LIBS="$LIBS -l${pthread_name} -lws2_32 -lregex"
	;;
*-*-solaris*)
	CPPFLAGS="$CPPFLAGS -D_POSIX_PTHREAD_SEMANTICS"
	CFLAGS="$CFLAGS -std=gnu99 -DSOLARIS"
	;;
esac
  
dnl Checks for iconv
AM_ICONV

dnl floor and ceil are  in -lm
LIBS="$LIBS -lm"

dnl
dnl Check for bind_textdomain_codeset, including -lintl if GLib brings it in.
dnl
syl_save_LIBS=$LIBS
LIBS="$LIBS $GTK_LIBS"
AC_CHECK_FUNCS(bind_textdomain_codeset)
LIBS=$syl_save_LIBS

dnl for gettext
ALL_LINGUAS="bg ca cs de en_GB es fi fr hu id_ID it ja lt nl pl pt_BR pt_PT ru sk sv uk zh_CN zh_TW"
GETTEXT_PACKAGE=claws-mail
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE,"$GETTEXT_PACKAGE", [Define text domain.])

AM_GNU_GETTEXT_VERSION([0.15])
AM_GNU_GETTEXT([external])

manualdir='${docdir}/manual'
AC_ARG_WITH(manualdir,
	[  --with-manualdir=DIR    Manual directory],
	[manualdir="$withval"])
AC_SUBST(manualdir)

AC_ARG_ENABLE(manual,
	[  --disable-manual           do not build manual],
	[ac_cv_enable_manual=$enableval], [ac_cv_enable_manual=yes])

dnl ******************************
dnl ** Check for required tools **
dnl ** to build manuals         **
dnl ******************************

AC_PATH_PROG(DOCBOOK2HTML, docbook2html)
AC_PATH_PROG(DOCBOOK2TXT, docbook2txt)
AC_PATH_PROG(DOCBOOK2PS, docbook2ps)
AC_PATH_PROG(DOCBOOK2PDF, docbook2pdf)

AM_CONDITIONAL(MANUAL_HTML, test -n "$DOCBOOK2HTML")
AM_CONDITIONAL(MANUAL_TXT, test -n "$DOCBOOK2TXT")
AM_CONDITIONAL(MANUAL_PDF, test -n "$DOCBOOK2PDF")
AM_CONDITIONAL(MANUAL_PS, test -n "$DOCBOOK2PS")

if test x"$ac_cv_enable_manual" = x"yes"; then
    if test -n "$DOCBOOK2TXT" -o -n "$DOCBOOK2HTML" \
	-o -n "$DOCBOOK2PS" -o -n "$DOCBOOK2PDF"; then
	    ac_cv_enable_manual=yes
	else
	    ac_cv_enable_manual=no
    fi
fi

AM_CONDITIONAL(BUILD_MANUAL, test x"$ac_cv_enable_manual" = xyes)

dnl Set PACKAGE_DATA_DIR in config.h.
if test "x${datarootdir}" = 'x${prefix}/share'; then
	if test "x${prefix}" = "xNONE"; then
    		AC_DEFINE_UNQUOTED(PACKAGE_DATA_DIR, "${ac_default_prefix}/share/${PACKAGE}",[PACKAGE_DATA_DIR])
  	else
    		AC_DEFINE_UNQUOTED(PACKAGE_DATA_DIR, "${prefix}/share/${PACKAGE}",[PACKAGE_DATA_DIR])
  	fi
else
	AC_DEFINE_UNQUOTED(PACKAGE_DATA_DIR, "${datarootdir}/${PACKAGE}",[PACKAGE_DATA_DIR])
fi

AC_CHECK_LIB(xpg4, setlocale)

SM_LIBS=""
dnl Check for LibSM
AC_ARG_ENABLE(libsm,
	[  --disable-libsm    disable libSM support for session management.],
	[ac_cv_enable_libsm=$enableval], [ac_cv_enable_libsm=yes])
AC_MSG_CHECKING([whether to use LibSM])
if test x"$ac_cv_enable_libsm" = xyes; then
	AC_MSG_RESULT(yes)
	AC_CHECK_LIB(SM, SmcSaveYourselfDone,
		[SM_LIBS="$X_LIBS -lSM -lICE"],ac_cv_enable_libsm=no,
		$X_LIBS -lICE)
	AC_CHECK_HEADERS(X11/SM/SMlib.h,,ac_cv_enable_libsm=no)
	if test x"$ac_cv_enable_libsm" = xyes; then
		AC_DEFINE(HAVE_LIBSM, 1, [Define to 1 if you have libSM installed])
	else
		AC_MSG_RESULT(not found)
		AC_MSG_WARN([*** LibSM will not be supported ***])
	fi
else
	AC_MSG_RESULT(no)
fi
AC_SUBST(SM_LIBS)

dnl for GThread support (currently disabled)
dnl AC_ARG_ENABLE(threads,
dnl	[  --enable-threads        Enable multithread support [default=no]],
dnl	[use_threads=$enableval], [use_threads=no])

AC_MSG_CHECKING([whether to use threads])
if test x"$use_threads" = xyes ; then
	AC_MSG_RESULT(yes)
	if test ! -z `$GLIB_CONFIG --help 2>&1 |grep 'gthread'` ; then
		CFLAGS="$CFLAGS `$GLIB_CONFIG --cflags gthread`"
		LIBS="$LIBS `$GLIB_CONFIG --libs gthread`"
		AC_DEFINE(USE_THREADS, 1, Whether to use multithread or not)
	else
		AC_MSG_ERROR([Claws Mail requires GThread from GLib to use threading.])
	fi
else
	AC_MSG_RESULT(no)
fi

dnl Check for d_type member in struct dirent
AC_MSG_CHECKING([whether struct dirent has d_type member])
AC_CACHE_VAL(ac_cv_dirent_d_type,[
	AC_TRY_COMPILE([#include <dirent.h>],
		       [struct dirent d; d.d_type = DT_REG;],
		       ac_cv_dirent_d_type=yes, ac_cv_dirent_d_type=no)
])
AC_MSG_RESULT($ac_cv_dirent_d_type)
if test $ac_cv_dirent_d_type = yes; then
	AC_DEFINE(HAVE_DIRENT_D_TYPE, 1,
		  Define if `struct dirent' has `d_type' member.)
fi

# Check whether mkdir does not take the permission argument.
GNUPG_FUNC_MKDIR_TAKES_ONE_ARG

dnl Checks for header files.
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS(fcntl.h sys/file.h unistd.h paths.h \
		 sys/param.h sys/utsname.h sys/select.h \
		 wchar.h wctype.h locale.h netdb.h)
AC_CHECK_HEADER([execinfo.h], [AC_DEFINE(HAVE_BACKTRACE,1,[Has backtrace*() needed for retrieving stack traces])])
AC_SEARCH_LIBS(backtrace_symbols, [execinfo])

dnl alf - Check for apache installation f*ck up. apache may also install an 
dnl fnmatch, which includes their own regex stuff if USE_HSREGEX is defined
AC_TRY_COMPILE([#include <stdlib.h>
		#include <fnmatch.h>],
	[int x = USE_HSREGEX;],
	ac_cv_have_apache_fnmatch=yes, ac_cv_have_apache_fnmatch=no)
if test $ac_cv_have_apache_fnmatch = yes; then
	AC_DEFINE(HAVE_APACHE_FNMATCH, 1, Define if you need to work around apache regex/fnmatch !KLUDGE!)
fi
AC_MSG_CHECKING([whether to use Apache regex header kludge])
AC_MSG_RESULT($ac_cv_have_apache_fnmatch)

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_STRUCT_TM

dnl AC_CHECK_TYPE(wint_t, unsigned int) does not work because wint_t
dnl may be defined only in wchar.h (this happens with gcc-2.96).
dnl So we need to use this extended macro.
CLAWS_CHECK_TYPE(wint_t, unsigned int,
[
#if HAVE_WCHAR_H
#include <wchar.h>
#endif
], Define to `unsigned int' if <stddef.h> or <wchar.h> doesn't define.)

GNUPG_CHECK_TYPEDEF(u32, HAVE_U32_TYPEDEF, Used to test for a u32 typedef)
AC_CHECK_SIZEOF(unsigned short, 2)
AC_CHECK_SIZEOF(unsigned int, 4)
AC_CHECK_SIZEOF(unsigned long, 4)

dnl Checks for library functions.
AC_FUNC_ALLOCA
AC_CHECK_FUNCS(gethostname mkdir mktime socket strstr strchr \
	       uname flock lockf inet_aton inet_addr \
	       fchmod mkstemp truncate getuid regcomp)

AC_CHECK_FUNCS(fgets_unlocked fwrite_unlocked)

dnl *****************
dnl ** common code **
dnl *****************

dnl check for glib
PKG_CHECK_MODULES(GLIB, glib-2.0 >= 2.6 gmodule-2.0 >= 2.6 gobject-2.0 >= 2.6 gthread-2.0 >= 2.6)
      
GLIB_GENMARSHAL=`pkg-config --variable=glib_genmarshal glib-2.0`
AC_SUBST(GLIB_GENMARSHAL)

AC_SUBST(GLIB_CFLAGS)
AC_SUBST(GLIB_LIBS)

dnl Check for bind_textdomain_codeset, including -lintl if GLib brings it in.
syl_save_LIBS=$LIBS
LIBS="$LIBS $GTK_LIBS"
AC_CHECK_FUNCS(bind_textdomain_codeset)
LIBS=$syl_save_LIBS

dnl check for IPv6 option
AC_ARG_ENABLE(ipv6,
	[  --disable-ipv6           disable build IPv6 support],
	[ac_cv_enable_ipv6=$enableval], [ac_cv_enable_ipv6=yes])

dnl automated checks for IPv6 support.
AC_MSG_CHECKING([whether to use IPv6])
if test x"$ac_cv_enable_ipv6" = xyes; then
	AC_MSG_RESULT(yes)
	AC_MSG_CHECKING([for IPv6 support])
	AC_CACHE_VAL(ac_cv_ipv6,[
		AC_TRY_COMPILE([#define INET6
				#include <sys/types.h>
				#include <netinet/in.h>],
			[int x = IPPROTO_IPV6; struct in6_addr a;],
			ac_cv_ipv6=yes, ac_cv_ipv6=no)
	])
	AC_MSG_RESULT($ac_cv_ipv6)
	if test $ac_cv_ipv6 = yes; then
		AC_DEFINE(INET6, 1, Define if you want IPv6 support.)
	else
		AC_MSG_WARN(*** IPv6 will not be supported ***)
		ac_cv_enable_ipv6=no
	fi
else
	AC_MSG_RESULT(no)
fi

dnl GNUTLS
AC_MSG_CHECKING([whether to use GnuTLS])
AC_ARG_ENABLE(gnutls,    [  --enable-gnutls      enable GnuTLS support],
            [ac_cv_enable_gnutls=$enableval], [ac_cv_enable_gnutls=yes])

AC_MSG_RESULT($ac_cv_enable_gnutls)
if test "x$ac_cv_enable_gnutls" != "xno"; then
        PKG_CHECK_MODULES(GNUTLS, gnutls >= 2.2,
        [
                AC_DEFINE(USE_GNUTLS, 1, gnutls)
                echo "Building with GnuTLS"
        ],
        [
                echo "Building without gnutls"
        ])
        PKG_CHECK_MODULES(GNUTLS, gnutls >= 2.11,
        [
                dnl No linking against libgcrypt needed
        ],
        [
                dnl linking against libgcrypt *is* needed
                GNUTLS_LIBS="$GNUTLS_LIBS -lgcrypt"
        ])
        AC_SUBST(GNUTLS_LIBS)
        AC_SUBST(GNUTLS_CFLAGS)
fi

dnl password encryption
OLDLIBS=$LIBS
LIBS=
case $host_os in
	*dragonfly*)
		AC_SEARCH_LIBS(encrypt, cipher, [], AC_MSG_ERROR(['encrypt'-function not found.]))
	;;
	*)
		AC_SEARCH_LIBS(encrypt, crypt, [], AC_MSG_ERROR(['encrypt'-function not found.]))
	;;
esac
CRYPT_LIBS=$LIBS
AC_SUBST(CRYPT_LIBS)
LIBS=$OLDLIBS

AC_ARG_WITH(passcrypt-key, [  --with-passcrypt-key=KEY     Key used to encode passwords (8 byte string)],
	    with_passcrypt_key="$withval", with_passcrypt_key="passkey0")
AC_SUBST(PASSCRYPT_KEY, $with_passcrypt_key)

dnl RC dir (will be default at a certain point in time)
AC_ARG_WITH(config-dir,    [  --with-config-dir=RCDIR      Local configuration dir (default: .claws-mail)],
	      ac_cv_with_config_dir="$withval", ac_cv_with_config_dir=".claws-mail")
if test x"$ac_cv_with_config_dir" = x""; then
	ac_cv_with_config_dir=".claws-mail"
fi 
AC_DEFINE_UNQUOTED(CFG_RC_DIR, "$ac_cv_with_config_dir", Configuration directory)

dnl ************************
dnl ** GTK user interface **
dnl ************************

dnl Checks for GTK
AC_ARG_ENABLE(gtk3,
	AS_HELP_STRING([--enable-gtk3],
		[Determines whether to use Gtk+ 3.0.]),
		[ac_cv_enable_gtk3=$enableval],[ac_cv_enable_gtk3=no])
AM_CONDITIONAL(GTK3, false)
AM_CONDITIONAL(GTK2, true)
if test x"$ac_cv_enable_gtk3" = x"yes"; then
	PKG_CHECK_MODULES(GTK, gtk+-3.0 >= 3.0 cairo)
	AM_CONDITIONAL(GTK3, true)
	AM_CONDITIONAL(GTK2, false)
else
	PKG_CHECK_MODULES(GTK, gtk+-2.0 >= 2.16)

	dnl --disable-deprecated switch for GTK2 purification
	AC_ARG_ENABLE(deprecated, [  --disable-deprecated  disable deprecated GTK functions. ],
			  [GTK_CFLAGS="$GTK_CFLAGS -DG_DISABLE_DEPRECATED -DGTK_DISABLE_DEPRECATED"], [])
fi
AC_SUBST(GTK_CFLAGS)
AC_SUBST(GTK_LIBS)

dnl enchant is used for spell checking
AC_MSG_CHECKING([whether to use enchant])
AC_ARG_ENABLE(enchant,
	[  --disable-enchant         disable enchant support],
	[ac_cv_enable_enchant=$enableval], [ac_cv_enable_enchant=yes])
AC_MSG_RESULT($ac_cv_enable_enchant)
if test $ac_cv_enable_enchant = yes; then
	PKG_CHECK_MODULES(ENCHANT, enchant >= 1.0.0,
	[
		AC_DEFINE(USE_ENCHANT, 1, enchant)
		echo "Building with enchant"
		ac_cv_enable_enchant=yes
		CFLAGS="$CFLAGS `$PKG_CONFIG --cflags enchant`"
	],
	[
		echo "Building without enchant-notification"
		ac_cv_enable_enchant=no
	])
	AC_SUBST(ENCHANT_CFLAGS)
	AC_SUBST(ENCHANT_LIBS)
fi

dnl want crash dialog
AC_ARG_ENABLE(crash-dialog,
	[  --enable-crash-dialog   Enable crash dialog [default=no]],
	[ac_cv_enable_crash_dialog=$enableval], [ac_cv_enable_crash_dialog=no])
if test $ac_cv_enable_crash_dialog = yes; then
dnl check if GDB is somewhere
	AC_CHECK_PROG(ac_cv_enable_crash_dialog, gdb, yes, no)
	AC_MSG_CHECKING([whether to use crash dialog])
	if test $ac_cv_enable_crash_dialog = yes; then
		AC_DEFINE(CRASH_DIALOG, 1, Pop up crash dialog)
	fi
	AC_MSG_RESULT($ac_cv_enable_crash_dialog)
fi

dnl generic umpc
AC_ARG_ENABLE(generic-umpc,
	[  --enable-generic-umpc   Enable generic UMPC code [default=no]],
	[ac_cv_enable_generic_umpc=$enableval], [ac_cv_enable_generic_umpc=no])
if test $ac_cv_enable_generic_umpc = yes; then
	AC_DEFINE(GENERIC_UMPC, 1, Generic UMPC code)
	AC_MSG_RESULT($ac_cv_enable_generic_umpc)
fi

dnl Maemo platform
AC_ARG_ENABLE(maemo,
	[  --enable-maemo   build for the Maemo platform [default=no]],
	[ac_cv_enable_maemo=$enableval], [ac_cv_enable_maemo=no])
if test $ac_cv_enable_maemo = yes; then
	PKG_CHECK_MODULES(MAEMO, libosso hildon-libs hildon-fm gnome-vfs-2.0, ac_cv_enable_maemo=yes,
		  ac_cv_enable_maemo=no)
	AC_SUBST(MAEMO_CFLAGS)
	AC_SUBST(MAEMO_LIBS)
	if test $ac_cv_enable_maemo = no; then
		#test for chinook
		PKG_CHECK_MODULES(MAEMO, libosso hildon-1 hildon-fm-2 gnome-vfs-2.0 hal, ac_cv_enable_maemo=yes,
			  ac_cv_enable_maemo=no)
		AC_SUBST(MAEMO_CFLAGS)
		AC_SUBST(MAEMO_LIBS)
		if test $ac_cv_enable_maemo = no; then
			AC_MSG_ERROR(one of libosso hildon-libs hildon-fm hildon-1 hildon-fm-2 not found)
		else
			AC_DEFINE(MAEMO, 1, Build for maemo)
			AC_DEFINE(CHINOOK, 1, Maemo chinook)
			AC_DEFINE(GENERIC_UMPC, 1, Generic UMPC code)
			ac_cv_enable_generic_umpc=yes
		fi
	else
		AC_DEFINE(MAEMO, 1, Build for maemo)
		AC_DEFINE(GENERIC_UMPC, 1, Generic UMPC code)
		ac_cv_enable_generic_umpc=yes
	fi
fi

PKG_CHECK_MODULES(CONIC, conic, ac_cv_enable_conic=yes,
	  ac_cv_enable_conic=no)
AC_SUBST(CONIC_CFLAGS)
AC_SUBST(CONIC_LIBS)
if test $ac_cv_enable_conic = yes; then
	AC_DEFINE(CONIC, 1, Have conic lib)
fi

dnl Check for X-Face support
AC_ARG_ENABLE(compface,
	[  --disable-compface      disable compface (X-Face) support],
	[ac_cv_enable_compface=$enableval], [ac_cv_enable_compface=yes])
AC_MSG_CHECKING([whether to use compface])
if test x"$ac_cv_enable_compface" = xyes; then
	AC_MSG_RESULT(yes)
	AC_CHECK_LIB(compface, uncompface, 
		[AC_DEFINE(HAVE_LIBCOMPFACE, 1, Define if you want compface support.)],
		[ac_cv_enable_compface=no])
	if test x"$ac_cv_enable_compface" = xyes; then
		COMPFACE_LIBS="-lcompface"
	else
		COMPFACE_LIBS=""
	fi
	AC_SUBST(COMPFACE_LIBS)
else
	AC_MSG_RESULT(no)
fi

dnl check for pthread support
AC_ARG_ENABLE(pthread,
	[  --disable-pthread           disable pthread support],
	[ac_cv_enable_pthread=$enableval], [ac_cv_enable_pthread=yes])
AC_MSG_CHECKING([whether to use pthread])
if test x$ac_cv_enable_pthread = xno; then
	AC_MSG_RESULT(no)
else
	AC_MSG_RESULT(yes)

        # For W32 we need to use a special ptrhead lib. In this case we can't
        # use AC_CHECK_LIB because it has no means of checking for a
        # library installed under a different name.  Checking for the
        # header is okay.
        if test -n "${pthread_name}" ; then
           ac_cv_enable_pthread=yes
        else
	AC_CHECK_LIB(pthread, pthread_create, :, ac_cv_enable_pthread=no)
        fi
	AC_CHECK_HEADERS(pthread.h, :, ac_cv_enable_pthread=no)

	if test x$ac_cv_enable_pthread = xyes; then
		AC_DEFINE(USE_PTHREAD, 1, Define if you have pthread)
                if test -z "${pthread_name}" ; then
		PTHREAD_LIBS="-lpthread"
	fi
	fi

fi
AC_SUBST(PTHREAD_LIBS)

dnl
dnl Check whether we need to pass -lresolv
dnl We know that we don't need it for W32.
dnl
if test x$os_win32 = xno; then
  t_oldLibs="$LIBS"
  LIBS="$LIBS"
  ac_cv_var__res_options=no
  AC_TRY_LINK([#include <sys/types.h>
 	     #include <sys/socket.h>
	     #include <netinet/in.h>
 	     #include <arpa/nameser.h>
	     #include <resolv.h>],
 	  	[_res.options = RES_INIT;],
 	 	ac_cv_var__res_options=yes);
  if test "$ac_cv_var__res_options" != "yes"; then
	LIBRESOLV="-lresolv"
  fi
  LIBS="$t_oldLibs"

  if test "x$LIBRESOLV" = "x"; then
	AC_CHECK_LIB(resolv, res_query, [LIBRESOLV=-lresolv])
	LIBS="$t_oldLibs"
  fi
fi
AC_SUBST(LIBRESOLV)

LIBS="$LIBS $LIBRESOLV"
 	  	 
dnl for LDAP support in addressbook
dnl no check for libraries; dynamically loaded
AC_ARG_ENABLE(ldap,
	[  --disable-ldap         disable LDAP support],
	[ac_cv_enable_ldap=$enableval], [ac_cv_enable_ldap=yes])
AC_MSG_CHECKING([whether to use LDAP])
if test x"$ac_cv_enable_ldap" = xno; then
	AC_MSG_RESULT(no)
elif test x"$ac_cv_enable_ldap" = xyes -a x"$ac_cv_enable_pthread" = xno; then
	AC_MSG_RESULT(no - LDAP support needs pthread support)

	ac_cv_enable_ldap=no
elif test x"$platform_win32" = xyes; then
	AC_MSG_RESULT(yes)
	AC_DEFINE(USE_LDAP, 1, Define if you want LDAP support in addressbook.)
	LDAP_LIBS="-lwldap32"
	AC_SUBST(LDAP_LIBS)
else
	AC_MSG_RESULT(yes)

	dnl check for available libraries, and pull them in
	AC_CHECK_LIB(resolv, res_query, LDAP_LIBS="$LDAP_LIBS -lresolv")
	AC_CHECK_LIB(socket, bind, LDAP_LIBS="$LDAP_LIBS -lsocket")
	AC_CHECK_LIB(nsl, gethostbyaddr, LDAP_LIBS="$LDAP_LIBS -lnsl")
	AC_CHECK_LIB(lber, ber_get_tag, LDAP_LIBS="$LDAP_LIBS -llber",,
		     $LDAP_LIBS)

	AC_CHECK_HEADERS(ldap.h lber.h,
			 [ ac_cv_enable_ldap=yes ],
			 [ ac_cv_enable_ldap=no ])

	if test "$ac_cv_enable_ldap" = yes; then
		AC_CHECK_LIB(ldap, ldap_open,
			     [ ac_cv_enable_ldap=yes ],
			     [ ac_cv_enable_ldap=no ],
			     $LDAP_LIBS)

		AC_CHECK_LIB(ldap, ldap_start_tls_s,
	     		     [ ac_cv_have_tls=yes ],
	     		     [ ac_cv_have_tls=no ])

	fi

	AC_MSG_CHECKING([whether ldap library is available])
	AC_MSG_RESULT($ac_cv_enable_ldap)

	AC_MSG_CHECKING([whether TLS library is available])
	AC_MSG_RESULT($ac_cv_have_tls)

	if test "$ac_cv_enable_ldap" = yes; then
		AC_DEFINE(USE_LDAP, 1, Define if you want LDAP support in addressbook.)
		LDAP_LIBS="$LDAP_LIBS -lldap"
		AC_SUBST(LDAP_LIBS)
		if test "$ac_cv_have_tls" = yes; then
			AC_DEFINE(USE_LDAP_TLS, 1, Define if you want LDAP TLS support in addressbook.)
		fi
		dnl As of OpenLDAP API version 3000 a number of functions has
		dnl been deprecated. As Claws-mail compiles and runs on many
		dnl platforms and many versions of OpenLDAP we need to be able
		dnl to switch between the old and new API because new API has
		dnl added new functions replacing old ones and at the same time
		dnl old functions has been changed.
		dnl If cross-compiling defaults to enable deprecated features
		dnl for maximum portability
		AC_MSG_CHECKING([The API version of OpenLDAP])
		AC_RUN_IFELSE(
			[AC_LANG_PROGRAM(
			 [#include <ldap.h>],
			 [if (LDAP_API_VERSION >= 3000)
					return 1
			])],
			[AC_MSG_RESULT([version < 3000])
			 AC_DEFINE(OPEN_LDAP_API_AT_LEAST_3000, 0, Define if OpenLDAP API is at least version 3000.)],
			[AC_MSG_RESULT([version >= 3000])
			 AC_DEFINE(OPEN_LDAP_API_AT_LEAST_3000, 1, Define if OpenLDAP API is at least version 3000.)],
			[AC_MSG_RESULT([Enabling deprecated features in OpenLDAP])
			 AC_DEFINE(OPEN_LDAP_API_AT_LEAST_3000, 0, Define if OpenLDAP API is at least version 3000.)
			 AC_DEFINE(LDAP_DEPRECATED, 1, Define to activate deprecated features in OpenLDAP)]
		)
	fi
fi

dnl for JPilot support in addressbook
dnl no check for libraries; these are dynamically loaded
AC_ARG_ENABLE(jpilot,
	[  --disable-jpilot         disable JPilot support],
	[ac_cv_enable_jpilot=$enableval], [ac_cv_enable_jpilot=yes])
AC_MSG_CHECKING([whether to use JPilot])
if test "$ac_cv_enable_jpilot" = yes; then
	AC_MSG_RESULT(yes)
	AC_CHECK_HEADERS(pi-args.h pi-appinfo.h pi-address.h pi-version.h,
			 [ AC_DEFINE(USE_JPILOT, 1, Define if you want JPilot support in addressbook.) ],
			 [ ac_cv_enable_jpilot=no ])
	if test "$ac_cv_enable_jpilot" = no; then
		AC_CHECK_HEADERS(libpisock/pi-args.h libpisock/pi-appinfo.h libpisock/pi-address.h libpisock/pi-version.h,
				 [ ac_cv_enable_jpilot=yes
				   AC_DEFINE(USE_JPILOT, 1, Define if you want JPilot support in addressbook.) ])
	fi

	AC_CHECK_LIB(pisock, unpack_Address, [JPILOT_LIBS="-lpisock"], [JPILOT_LIBS="" ac_cv_enable_jpilot="no"])
	if test x"$ac_cv_enable_jpilot" = xyes; then
		AC_DEFINE(USE_JPILOT, 1, Define if you want JPilot support in addressbook.)
	else
		AC_MSG_NOTICE([JPilot support not available])
	fi
	AC_SUBST(JPILOT_LIBS)
else
	AC_MSG_RESULT(no)
fi

dnl #######################################################################
dnl # Check for startup notification
dnl #######################################################################
AC_ARG_ENABLE(startup-notification, [  --disable-startup-notification    disable startup notification support],,enable_startup_notification=yes)

if test "x$enable_startup_notification" = "xyes"; then
	PKG_CHECK_MODULES(STARTUP_NOTIFICATION, libstartup-notification-1.0 >= 0.5,
	[
		AC_DEFINE(HAVE_STARTUP_NOTIFICATION, 1, [Define if we're using libstartup-notification.])
		echo "Building with libstartup-notification"
		enable_startup_notification=yes
	],
	[
		echo "Building without libstartup-notification"
		enable_startup_notification=no
	])

	AC_SUBST(STARTUP_NOTIFICATION_CFLAGS)
	AC_SUBST(STARTUP_NOTIFICATION_LIBS)
fi

dnl #######################################################################
dnl # Check for D-Bus support
dnl #######################################################################
AC_ARG_ENABLE(dbus, [  --disable-dbus    disable dbus support],,enable_dbus=yes)

if test "x$enable_dbus" = "xyes"; then
	PKG_CHECK_MODULES(DBUS, [dbus-1 >= 0.60, dbus-glib-1 >= 0.60],
	[
		AC_DEFINE(HAVE_DBUS_GLIB, 1, [Define if glib bindings of D-Bus are available])
		enable_dbus_glib=yes
	],
	[
		echo "D-Bus requirements not met. D-Bus support not activated."
		enable_dbus_glib=no
	])
	AC_SUBST(DBUS_CFLAGS)
	AC_SUBST(DBUS_LIBS)
fi

dnl #######################################################################
dnl # Configure address book support
dnl #######################################################################

dnl #######################################################################
dnl # Check for new address book support
dnl #######################################################################
AC_MSG_CHECKING([whether DBUS support for new address book is present])
if test x"$enable_dbus_glib" = xyes; then
	AC_MSG_RESULT([yes])
	AC_MSG_CHECKING([whether to enable new address book])
	AC_ARG_ENABLE(new-addrbook, [  --enable-new-addrbook     enable new address book],,enable_new_addrbook=no)
	if test x"$enable_new_addrbook" = xyes; then
		AC_MSG_RESULT([yes])
		PKG_CHECK_MODULES(CONTACTS, [claws-contacts],
		[
			AC_DEFINE(USE_NEW_ADDRBOOK, 1, [Define if new address book is to be activated.])
			enable_new_addrbook=yes
			AC_SUBST(CONTACTS_CFLAGS)
			AC_SUBST(CONTACTS_LIBS)
		],
		[
			enable_new_addrbook=no
		])
	else
		AC_MSG_RESULT([no])
		enable_new_addrbook=no
	fi
else
	AC_MSG_RESULT([no])
	enable_new_addrbook=no
fi

dnl #######################################################################
dnl # Check for old address book support
dnl #######################################################################
if test x"$enable_new_addrbook" = xno; then
	dnl for LDAP support in addressbook
	dnl no check for libraries; dynamically loaded
	AC_ARG_ENABLE(ldap,
		[  --disable-ldap         disable LDAP support],
		[ac_cv_enable_ldap=$enableval], [ac_cv_enable_ldap=yes])
	AC_MSG_CHECKING([whether to use LDAP])
	if test x"$ac_cv_enable_ldap" = xno; then
		AC_MSG_RESULT(no)
	elif test x"$ac_cv_enable_ldap" = xyes -a x"$ac_cv_enable_pthread" = xno; then
		AC_MSG_RESULT(no - LDAP support needs pthread support)

		ac_cv_enable_ldap=no
	elif test x"$platform_win32" = xyes; then
		AC_MSG_RESULT(yes)
		AC_DEFINE(USE_LDAP, 1, Define if you want LDAP support in addressbook.)
		LDAP_LIBS="-lwldap32"
		AC_SUBST(LDAP_LIBS)
	else
		AC_MSG_RESULT(yes)

		dnl check for available libraries, and pull them in
		AC_CHECK_LIB(resolv, res_query, LDAP_LIBS="$LDAP_LIBS -lresolv")
		AC_CHECK_LIB(socket, bind, LDAP_LIBS="$LDAP_LIBS -lsocket")
		AC_CHECK_LIB(nsl, gethostbyaddr, LDAP_LIBS="$LDAP_LIBS -lnsl")
		AC_CHECK_LIB(lber, ber_get_tag, LDAP_LIBS="$LDAP_LIBS -llber",,
				 $LDAP_LIBS)

		AC_CHECK_HEADERS(ldap.h lber.h,
				 [ ac_cv_enable_ldap=yes ],
				 [ ac_cv_enable_ldap=no ])

		if test "$ac_cv_enable_ldap" = yes; then
			AC_CHECK_LIB(ldap, ldap_open,
					 [ ac_cv_enable_ldap=yes ],
					 [ ac_cv_enable_ldap=no ],
					 $LDAP_LIBS)

			AC_CHECK_LIB(ldap, ldap_start_tls_s,
			 		     [ ac_cv_have_tls=yes ],
			 		     [ ac_cv_have_tls=no ])

		fi

		AC_MSG_CHECKING([whether ldap library is available])
		AC_MSG_RESULT($ac_cv_enable_ldap)

		AC_MSG_CHECKING([whether TLS library is available])
		AC_MSG_RESULT($ac_cv_have_tls)

		if test "$ac_cv_enable_ldap" = yes; then
			AC_DEFINE(USE_LDAP, 1, Define if you want LDAP support in addressbook.)
			LDAP_LIBS="$LDAP_LIBS -lldap"
			AC_SUBST(LDAP_LIBS)
			if test "$ac_cv_have_tls" = yes; then
				AC_DEFINE(USE_LDAP_TLS, 1, Define if you want LDAP TLS support in addressbook.)
			fi
			dnl As of OpenLDAP API version 3000 a number of functions has
			dnl been deprecated. As Claws-mail compiles and runs on many
			dnl platforms and many versions of OpenLDAP we need to be able
			dnl to switch between the old and new API because new API has
			dnl added new functions replacing old ones and at the same time
			dnl old functions has been changed.
			dnl If cross-compiling defaults to enable deprecated features
			dnl for maximum portability
			AC_MSG_CHECKING([The API version of OpenLDAP])
			AC_RUN_IFELSE(
				[AC_LANG_PROGRAM(
				 [#include <ldap.h>],
				 [if (LDAP_API_VERSION >= 3000)
						return 1
				])],
				[AC_MSG_RESULT([version < 3000])
				 AC_DEFINE(OPEN_LDAP_API_AT_LEAST_3000, 0, Define if OpenLDAP API is at least version 3000.)],
				[AC_MSG_RESULT([version >= 3000])
				 AC_DEFINE(OPEN_LDAP_API_AT_LEAST_3000, 1, Define if OpenLDAP API is at least version 3000.)],
				[AC_MSG_RESULT([Enabling deprecated features in OpenLDAP])
				 AC_DEFINE(OPEN_LDAP_API_AT_LEAST_3000, 0, Define if OpenLDAP API is at least version 3000.)
				 AC_DEFINE(LDAP_DEPRECATED, 1, Define to activate deprecated features in OpenLDAP)]
			)
		fi
	fi

	dnl for JPilot support in addressbook
	dnl no check for libraries; these are dynamically loaded
	AC_ARG_ENABLE(jpilot,
		[  --disable-jpilot         disable JPilot support],
		[ac_cv_enable_jpilot=$enableval], [ac_cv_enable_jpilot=yes])
	AC_MSG_CHECKING([whether to use JPilot])
	if test "$ac_cv_enable_jpilot" = yes; then
		AC_MSG_RESULT(yes)
		AC_CHECK_HEADERS(pi-args.h pi-appinfo.h pi-address.h pi-version.h,
				 [ AC_DEFINE(USE_JPILOT, 1, Define if you want JPilot support in addressbook.) ],
				 [ ac_cv_enable_jpilot=no ])
		if test "$ac_cv_enable_jpilot" = no; then
			AC_CHECK_HEADERS(libpisock/pi-args.h libpisock/pi-appinfo.h libpisock/pi-address.h libpisock/pi-version.h,
					 [ ac_cv_enable_jpilot=yes
					   AC_DEFINE(USE_JPILOT, 1, Define if you want JPilot support in addressbook.) ])
		fi

		AC_CHECK_LIB(pisock, unpack_Address, [JPILOT_LIBS="-lpisock"], [JPILOT_LIBS="" ac_cv_enable_jpilot="no"])
		if test x"$ac_cv_enable_jpilot" = xyes; then
			AC_DEFINE(USE_JPILOT, 1, Define if you want JPilot support in addressbook.)
		else
			AC_MSG_NOTICE([JPilot support not available])
		fi
		AC_SUBST(JPILOT_LIBS)
	else
		AC_MSG_RESULT(no)
	fi
fi

AM_CONDITIONAL(BUILD_NEWADDRBOOK, test x"$enable_new_addrbook" = x"yes")

dnl #######################################################################
dnl # Check for NetworkManager support
dnl #######################################################################
if test x"$enable_dbus_glib" = xyes; then
	AC_ARG_ENABLE(networkmanager-support, [  --disable-networkmanager-support  disable NetworkManager support],,enable_networkmanager_support=yes)
	if test x"$enable_networkmanager_support" = xyes; then
		PKG_CHECK_MODULES(NETWORKMANAGER_SUPPORT, NetworkManager >= 0.6.2,
		[
			AC_DEFINE(HAVE_NETWORKMANAGER_SUPPORT, 1, [Define if NetworkManager support is to be included.])
			echo "Building with NetworkManager support"
			enable_networkmanager_support=yes
		],
		[
			echo "NetworkManager not found."
			enable_networkmanager_support=no
		])
		AC_SUBST(NETWORKMANAGER_SUPPORT_CFLAGS)
	fi
else
	echo "NetworkManager support deactivated as D-Bus requirements were not met."
	enable_networkmanager_support=no
fi

dnl *************************
dnl ** section for plugins **
dnl *************************

PLUGINS=""

dnl --- Trayicon ---
AC_MSG_CHECKING([whether to build Trayicon plugin])
AC_ARG_ENABLE(trayicon-plugin,
	[  --disable-trayicon-plugin         do not build System Tray Icon plugin],
	[ac_cv_enable_trayicon_plugin=$enableval], [ac_cv_enable_trayicon_plugin=yes])
if test x"$ac_cv_enable_trayicon_plugin" = xyes; then
	AC_MSG_RESULT(yes)
	PLUGINS="trayicon $PLUGINS"
else
	AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(BUILD_TRAYICON_PLUGIN, test x"$ac_cv_enable_trayicon_plugin" = xyes)

dnl --- SpamAssassin ---
AC_MSG_CHECKING([whether to build SpamAssassin plugin])
AC_ARG_ENABLE(spamassassin-plugin,
	[  --disable-spamassassin-plugin      do not build SpamAssassin plugin],
	[ac_cv_enable_spamassassin_plugin=$enableval], [ac_cv_enable_spamassassin_plugin=yes])
if test x"$ac_cv_enable_spamassassin_plugin" = xyes; then
	AC_MSG_RESULT(yes)
	AC_SPAMASSASSIN
	PLUGINS="spamassassin $PLUGINS"
	AC_DEFINE(USE_SPAMASSASSIN_PLUGIN, 1, Define if spamassassin plugin is being built.)
else
	AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(BUILD_SPAMASSASSIN_PLUGIN, test x"$ac_cv_enable_spamassassin_plugin" = xyes)

dnl --- Bogofilter ---
AC_MSG_CHECKING([whether to build Bogofilter plugin])
AC_ARG_ENABLE(bogofilter-plugin,
	[  --disable-bogofilter-plugin      do not build bogofilter plugin],
	[ac_cv_enable_bogofilter_plugin=$enableval], [ac_cv_enable_bogofilter_plugin=yes])
if test x"$ac_cv_enable_bogofilter_plugin" = xyes; then
	AC_MSG_RESULT(yes)
	PLUGINS="bogofilter $PLUGINS"
	AC_DEFINE(USE_BOGOFILTER_PLUGIN, 1, Define if bogofilter plugin is being built.)
else
	AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(BUILD_BOGOFILTER_PLUGIN, test x"$ac_cv_enable_bogofilter_plugin" = xyes)

dnl --- PGP/CORE ---
AC_MSG_CHECKING([whether to build PGP/CORE plugin])
AC_ARG_ENABLE(pgpcore-plugin,
	[  --disable-pgpcore-plugin           do not build PGP/Core plugin],
	[ac_cv_enable_pgpcore_plugin=$enableval], [ac_cv_enable_pgpcore_plugin=yes])
if test x"$ac_cv_enable_pgpcore_plugin" = xyes; then
	AC_MSG_RESULT(yes)
	AM_PATH_GPGME(1.0.0, AC_DEFINE(USE_GPGME, 1, Define if you use GPGME to support OpenPGP.),
		      [ac_cv_enable_pgpcore_plugin=no])
	if test x"$ac_cv_enable_pgpcore_plugin" = xyes; then
		PLUGINS="pgpcore $PLUGINS"
                AM_PATH_GPGME(1.1.1,
                              AC_DEFINE(HAVE_GPGME_PKA_TRUST, 1,
                                        [Define if GPGME supports PKA.]))
		#needed to get GPGME_LIBS and al correctly
		AM_PATH_GPGME(1.0.0, AC_DEFINE(USE_GPGME, 1, Define if you use GPGME to support OpenPGP.),
		    	  [ac_cv_enable_pgpcore_plugin=no])
	else
		AC_MSG_WARN([*** PGP/CORE plugin will not be built ***])
	fi
else
	AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(BUILD_PGPCORE_PLUGIN, test x"$ac_cv_enable_pgpcore_plugin" = xyes)

AC_MSG_CHECKING([whether to build PGP/MIME plugin])
AC_ARG_ENABLE(pgpmime-plugin,
	[  --disable-pgpmime-plugin           do not build PGP/MIME plugin],
	[ac_cv_enable_pgpmime_plugin=$enableval], [ac_cv_enable_pgpmime_plugin=yes])
if test x"$ac_cv_enable_pgpmime_plugin" = xyes; then
	AC_MSG_RESULT(yes)
	if test x"$ac_cv_enable_pgpcore_plugin" = xyes; then
		PLUGINS="pgpmime $PLUGINS"
	else
		AC_MSG_WARN([*** PGP/MIME plugin cannot be built ***])
		AC_MSG_WARN([*** without the PGP/CORE plugin     ***])
	fi
else
	AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(BUILD_PGPMIME_PLUGIN, test x"$ac_cv_enable_pgpmime_plugin" = xyes)

AC_MSG_CHECKING([whether to build PGP/Inline plugin])
AC_ARG_ENABLE(pgpinline-plugin,
	[  --disable-pgpinline-plugin           do not build PGP/Inline plugin],
	[ac_cv_enable_pgpinline_plugin=$enableval], [ac_cv_enable_pgpinline_plugin=yes])
if test x"$ac_cv_enable_pgpinline_plugin" = xyes; then
	AC_MSG_RESULT(yes)
	if test x"$ac_cv_enable_pgpcore_plugin" = xyes; then
		PLUGINS="pgpinline $PLUGINS"
	else
		AC_MSG_WARN([*** PGP/Inline plugin cannot be built ***])
		AC_MSG_WARN([*** without the PGP/CORE plugin     ***])
	fi
else
	AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(BUILD_PGPINLINE_PLUGIN, test x"$ac_cv_enable_pgpinline_plugin" = xyes)

AC_MSG_CHECKING([whether to build S/Mime plugin])
AC_ARG_ENABLE(smime-plugin,
	[  --disable-smime-plugin           do not build S/Mime plugin],
	[ac_cv_enable_smime_plugin=$enableval], [ac_cv_enable_smime_plugin=yes])
if test x"$ac_cv_enable_smime_plugin" = xyes; then
	AC_MSG_RESULT(yes)
	if test x"$ac_cv_enable_pgpcore_plugin" = xyes; then
		PLUGINS="smime $PLUGINS"
	else
		AC_MSG_WARN([*** S/Mime plugin cannot be built ***])
		AC_MSG_WARN([*** without the PGP/CORE plugin     ***])
	fi
else
	AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(BUILD_SMIME_PLUGIN, test x"$ac_cv_enable_smime_plugin" = xyes)

dnl --- Dillo Viewer ---
AC_MSG_CHECKING([whether to build Dillo plugin])
AC_ARG_ENABLE(dillo-viewer-plugin,
	[  --disable-dillo-viewer-plugin     do not build Dillo plugin for html mail rendering],
	[ac_cv_enable_dillo_viewer_plugin=$enableval], [ac_cv_enable_dillo_viewer_plugin=yes])
if test x"$ac_cv_enable_dillo_viewer_plugin" = xyes; then
	AC_MSG_RESULT(yes)
	PLUGINS="dillo-viewer $PLUGINS"
else
	AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(BUILD_DILLO_VIEWER_PLUGIN, test x"$ac_cv_enable_dillo_viewer_plugin" = xyes)

dnl --- Demo ---
AC_ARG_ENABLE(demo-plugin,
	[  --enable-demo-plugin              build demo plugin],
	[ac_cv_enable_demo_plugin=$enableval], [ac_cv_enable_demo_plugin=no])
if test x"$ac_cv_enable_demo_plugin" = xyes; then
	PLUGINS="demo $PLUGINS"
fi
AM_CONDITIONAL(BUILD_DEMO_PLUGIN, test x"$ac_cv_enable_demo_plugin" = xyes)

dnl Libetpan
AC_MSG_CHECKING([whether to use libetpan])
AC_ARG_ENABLE(libetpan,
	[  --disable-libetpan		disable IMAP4/NNTP (libetpan) support],
	[ac_cv_enable_libetpan=$enableval], [ac_cv_enable_libetpan=yes])
if test x"$ac_cv_enable_libetpan" = xyes; then
	AC_MSG_RESULT(yes)
	libetpan_result=no
	AC_PATH_PROG(libetpanconfig, [libetpan-config])
	if test "x$libetpanconfig" != "x"; then
	  CPPFLAGS="$CPPFLAGS `$libetpanconfig --cflags 2>/dev/null`"
	  AC_CHECK_HEADER(libetpan/libetpan.h, [libetpan_result=yes])
	  if test "x$libetpan_result" = "xyes"; then
	    AC_MSG_CHECKING([whether libetpan-config hints compiles and links fine])
	    LIBS="$LIBS `$libetpanconfig --libs 2>/dev/null`"
	    AC_TRY_LINK([#include <libetpan/dbstorage.h>], [db_mailstorage_init(NULL, NULL);], [libetpan_result=yes], [libetpan_result=no])
	    AC_MSG_RESULT([$libetpan_result])
	  fi
	fi
	if test "x$libetpan_result" = "xyes"; then
	   LIBETPAN_CPPFLAGS="`$libetpanconfig --cflags`"
	   LIBETPAN_LIBS="`$libetpanconfig --libs`"
           LIBETPAN_STABLE=`$libetpanconfig --version | grep -v ^0`
	   LIBETPAN_VERSION=`$libetpanconfig --version | sed "s/\.//g" | sed "s/-.*$//"`
           if test x"$LIBETPAN_STABLE" != "x"; then
                LIBETPAN_VERSION="100"
           fi
	   if test "$LIBETPAN_VERSION" -lt "057"; then
		AC_MSG_RESULT([*** Claws Mail requires libetpan 0.57 or newer. See http://www.etpan.org/])
		AC_MSG_RESULT([*** You can use --disable-libetpan if you don't need IMAP4 and/or NNTP support.])
                AC_MSG_ERROR([libetpan 0.57 not found])
	   fi
	   AC_SUBST(LIBETPAN_FLAGS)
	   AC_SUBST(LIBETPAN_LIBS)
	   AC_DEFINE(HAVE_LIBETPAN, 1, Define if you want IMAP and/or NNTP support.)
	else
	   AC_MSG_RESULT([*** Claws Mail requires libetpan 0.57 or newer. See http://www.etpan.org/ ])
	   AC_MSG_RESULT([*** You can use --disable-libetpan if you don't need IMAP4 and/or NNTP support.])
           AC_MSG_ERROR([libetpan 0.57 not found])
	fi
else
	AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(CLAWS_LIBETPAN, test "x$libetpan_result" = "xyes")

AC_MSG_CHECKING([whether to use valgrind])
AC_ARG_ENABLE(valgrind,
	[  --disable-valgrind		disable valgrind support for debugging],
	[ac_cv_enable_valgrind=$enableval], [ac_cv_enable_valgrind=yes])
if test x$ac_cv_enable_valgrind = xyes; then
	AC_MSG_RESULT(yes)
	PKG_CHECK_MODULES(VALGRIND, valgrind >= 2.4.0,
			  ac_cv_enable_valgrind=yes, ac_cv_enable_valgrind=no)
	if test x"$ac_cv_enable_valgrind" = xyes; then
		AC_DEFINE(HAVE_VALGRIND, 1, Define if you want valgrind support)
	else
		AC_MSG_RESULT(not found)
	fi
else
	AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(CLAWS_VALGRIND, test x"$ac_cv_enable_valgrind" = x"yes")

dnl ****************************
dnl ** Final configure output **
dnl ****************************

AC_OUTPUT([
Makefile
m4/Makefile
po/Makefile.in
src/common/version.h
src/Makefile
src/common/Makefile
src/common/passcrypt.h
src/gtk/Makefile
src/etpan/Makefile
src/plugins/Makefile
src/plugins/demo/Makefile
src/plugins/bogofilter/Makefile
src/plugins/spamassassin/Makefile
src/plugins/dillo_viewer/Makefile
src/plugins/trayicon/Makefile
src/plugins/pgpcore/Makefile
src/plugins/pgpmime/Makefile
src/plugins/pgpinline/Makefile
src/plugins/smime/Makefile
doc/Makefile
doc/man/Makefile
tools/Makefile
config/Makefile
manual/Makefile
manual/dtd/Makefile
manual/dist/Makefile
manual/dist/pdf/Makefile
manual/dist/ps/Makefile
manual/dist/html/Makefile
manual/dist/txt/Makefile
manual/fr/Makefile
manual/fr/dist/Makefile
manual/fr/dist/pdf/Makefile
manual/fr/dist/ps/Makefile
manual/fr/dist/html/Makefile
manual/fr/dist/txt/Makefile
manual/pl/Makefile
manual/pl/dist/Makefile
manual/pl/dist/pdf/Makefile
manual/pl/dist/ps/Makefile
manual/pl/dist/html/Makefile
manual/pl/dist/txt/Makefile
manual/es/Makefile
manual/es/dist/Makefile
manual/es/dist/pdf/Makefile
manual/es/dist/ps/Makefile
manual/es/dist/html/Makefile
manual/es/dist/txt/Makefile
manual/de/Makefile
manual/de/dist/Makefile
manual/de/dist/pdf/Makefile
manual/de/dist/ps/Makefile
manual/de/dist/html/Makefile
manual/de/dist/txt/Makefile
claws-mail.pc
])

dnl Output the configuration summary
echo ""
echo "$PACKAGE $VERSION"
echo ""
echo "JPilot            : $ac_cv_enable_jpilot"
echo "LDAP              : $ac_cv_enable_ldap"
echo "gnuTLS            : $ac_cv_enable_gnutls"
echo "iconv             : $am_cv_func_iconv"
echo "compface          : $ac_cv_enable_compface"
echo "IPv6              : $ac_cv_enable_ipv6"
echo "enchant           : $ac_cv_enable_enchant"
echo "IMAP4             : $ac_cv_enable_libetpan"
echo "NNTP              : $ac_cv_enable_libetpan"
echo "Crash dialog      : $ac_cv_enable_crash_dialog"
echo "LibSM             : $ac_cv_enable_libsm"
echo "DBUS              : $enable_dbus"
echo "NetworkManager    : $enable_networkmanager_support"
echo "Manual            : $ac_cv_enable_manual"
echo "Plugins           : $PLUGINS"
echo "Generic UMPC code : $ac_cv_enable_generic_umpc"
echo "Maemo  build      : $ac_cv_enable_maemo"
echo "Config dir        : $ac_cv_with_config_dir"
echo ""
echo "The binary will be installed in $prefix/bin"
echo ""
echo "Configure finished, type 'make' to build."
