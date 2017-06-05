dnl Autoconf macros for libgpgme
dnl $Id: gpgme.m4,v 1.1.4.1 2004-05-05 08:47:50 twb Exp $

# Configure paths for GPGME
# Shamelessly stolen from the one of XDELTA by Owen Taylor
# Werner Koch  2000-11-17

dnl AM_PATH_GPGME([MINIMUM-VERSION,
dnl               [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND ]]])
dnl Test for gpgme, and define GPGME_CFLAGS and GPGME_LIBS
dnl
AC_DEFUN([AM_PATH_GPGME],
[dnl
dnl Get the cflags and libraries from the gpgme-config script
dnl
  AC_ARG_WITH(gpgme-prefix,
   [  --with-gpgme-prefix=PFX   Prefix where gpgme is installed (optional)],
          gpgme_config_prefix="$withval", gpgme_config_prefix="")
  AC_ARG_ENABLE(gpgmetest,
   [  --disable-gpgmetest    Do not try to compile and run a test gpgme program],
          , enable_gpgmetest=yes)

  if test x$gpgme_config_prefix != x ; then
     gpgme_config_args="$gpgme_config_args --prefix=$gpgme_config_prefix"
     if test x${GPGME_CONFIG+set} != xset ; then
        GPGME_CONFIG=$gpgme_config_prefix/bin/gpgme-config
     fi
  fi

  AC_PATH_PROG(GPGME_CONFIG, gpgme-config, no)
  min_gpgme_version=ifelse([$1], ,1.0.0,$1)
  AC_MSG_CHECKING(for GPGME - version >= $min_gpgme_version)
  no_gpgme=""
  if test "$GPGME_CONFIG" = "no" ; then
    no_gpgme=yes
  else
    GPGME_CFLAGS=`$GPGME_CONFIG $gpgme_config_args --cflags`
    GPGME_LIBS=`$GPGME_CONFIG $gpgme_config_args --libs`
    gpgme_config_version=`$GPGME_CONFIG $gpgme_config_args --version`
    if test "x$enable_gpgmetest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $GPGME_CFLAGS"
      LIBS="$LIBS $GPGME_LIBS"
dnl
dnl Now check if the installed gpgme is sufficiently new. Also sanity
dnl checks the results of gpgme-config to some extent
dnl
      rm -f conf.gpgmetest
      AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gpgme.h>

int
main ()
{
 system ("touch conf.gpgmetest");

 if( strcmp( gpgme_check_version(NULL), "$gpgme_config_version" ) )
 {
   printf("\n"
"*** 'gpgme-config --version' returned %s, but GPGME (%s) was found!\n",
              "$gpgme_config_version", gpgme_check_version(NULL) );
   printf(
"*** If gpgme-config was correct, then it is best to remove the old\n"
"*** version of GPGME.  You may also be able to fix the error\n"
"*** by modifying your LD_LIBRARY_PATH enviroment variable, or by editing\n"
"*** /etc/ld.so.conf.  Make sure you have run ldconfig if that is\n"
"*** required on your system.\n"
"*** If gpgme-config was wrong, set the environment variable GPGME_CONFIG\n"
"*** to point to the correct copy of gpgme-config, \n"
"*** and remove the file config.cache before re-running configure\n"
        );
 }
 else if ( strcmp(gpgme_check_version(NULL), GPGME_VERSION ) )
 {
   printf("\n*** GPGME header file (version %s) does not match\n",
            GPGME_VERSION);
   printf("*** library (version %s)\n", gpgme_check_version(NULL) );
 }
 else
 {
        if ( gpgme_check_version( "$min_gpgme_version" ) )
             return 0;
  printf("no\n"
"*** An old version of GPGME (%s) was found.\n", gpgme_check_version(NULL) );
  printf(
"*** You need a version of GPGME newer than %s.\n", "$min_gpgme_version" );
  printf(
"*** The latest version of GPGME is always available at\n"
"***      ftp://ftp.gnupg.org/pub/gcrypt/alpha/gpgme/\n"
"*** \n"
"*** If you have already installed a sufficiently new version, this error\n"
"*** probably means that the wrong copy of the gpgme-config shell script is\n"
"*** being found. The easiest way to fix this is to remove the old version\n"
"*** of GPGME, but you can also set the GPGME_CONFIG environment to point to\n"
"*** the correct copy of gpgme-config. (In this case, you will have to\n"
"*** modify your LD_LIBRARY_PATH enviroment variable, or edit /etc/ld.so.conf\n"
"*** so that the correct libraries are found at run-time).\n"
      );
    }
  return 1;
}
],, no_gpgme=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi
  if test "x$no_gpgme" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])
  else
     if test -f conf.gpgmetest ; then
        :
     else
        AC_MSG_RESULT(no)
     fi
     if test "$GPGME_CONFIG" = "no" ; then
       echo "*** The gpgme-config script installed by GPGME could not be found"
       echo "*** If GPGME was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the GPGME_CONFIG environment variable to the"
       echo "*** full path to gpgme-config."
     else
       if test -f conf.gpgmetest ; then
        :
       else
          echo "*** Could not run gpgme test program, checking why..."
          CFLAGS="$CFLAGS $GPGME_CFLAGS"
          LIBS="$LIBS $GPGME_LIBS"
          AC_TRY_LINK([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gpgme.h>
],      [ gpgme_check_version(NULL); return 0 ],
        [ 
echo "*** The test program compiled, but did not run. This usually means"
echo "*** that the run-time linker is not finding GPGME or finding the wrong"
echo "*** version of GPGME. If it is not finding GPGME, you'll need to set your"
echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
echo "*** to the installed location  Also, make sure you have run ldconfig if"
echo "*** that is required on your system"
echo "***"
echo "*** If you have an old version installed, it is best to remove it,"
echo "*** although you may also be able to get things to work by"
echo "*** modifying LD_LIBRARY_PATH"
echo "***"
        ],
        [
echo "*** The test program failed to compile or link. See the file config.log"
echo "*** for the exact error that occured. This usually means GPGME was"
echo "*** incorrectly installed or that you have moved GPGME since it was"
echo "*** installed. In the latter case, you may want to edit the"
echo "*** gpgme-config script: $GPGME_CONFIG" 
        ])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     GPGME_CFLAGS=""
     GPGME_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(GPGME_CFLAGS)
  AC_SUBST(GPGME_LIBS)
  rm -f conf.gpgmetest
])

