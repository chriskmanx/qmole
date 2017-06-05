dnl Autoconf macros for libaspell
dnl $Id: aspell.m4,v 1.1.4.1 2004-05-05 08:46:03 twb Exp $

# Configure paths for ASPELL
# Shamelessly stolen from the one of GPGME by Werner Koch 
# Melvin Hadasht  2001-09-17, 2002

dnl AM_PATH_ASPELL([MINIMUM-VERSION,
dnl               [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND ]]])
dnl Test for aspell, and define ASPELL_CFLAGS, ASPELL_LIBS and ASPELL_PATH
dnl
AC_DEFUN([AM_PATH_ASPELL],
[dnl
dnl Get the cflags and libraries from the aspell-config script
dnl
   AC_ARG_WITH(aspell-prefix,
      [  --with-aspell-prefix=PFX           Prefix where aspell is installed (optional)],
      aspell_prefix="$withval", aspell_prefix="")
   AC_ARG_ENABLE(aspell-test,
      [  --disable-aspell-test   Do not try to compile and run a test GNU/aspell program],
      , enable_aspelltest=yes)
   AC_ARG_WITH(aspell-libs,
      [  --with-aspell-libs=LIBS            Where GNU/aspell library reside (/usr/local/lib)],
      aspell_libs="$withval", aspell_libs="")
   AC_ARG_WITH(aspell-includes,
      [  --with-aspell-includes=INCLUDES    Where GNU/aspell headers reside (/usr/local/include)],
      aspell_includes="$withval", aspell_includes="")

  if test x$aspell_prefix != x ; then
     if test x${ASPELL+set} != xset ; then
        ASPELL=$aspell_prefix/bin/aspell
     fi
     if test x$aspell_includes = x ; then
        aspell_includes=$aspell_prefix/include
     fi
     if test x$aspell_libs = x ; then
        aspell_libs=$aspell_prefix/lib
     fi
     aspell_path=$aspell_prefix/lib/aspell
  fi
  if test x$aspell_includes = x ; then
     aspell_includes=/usr/local/include
  fi
  if test x$aspell_libs = x ; then
     aspell_libs=/usr/local/lib
  fi
  if test x$aspell_path = x ; then
     aspell_path=/usr/local/lib/aspell
  fi
  if test "x$enable_aspelltest" != "xyes" ; then
     echo "*** Disabling GNU/aspell tests upon user request" 
     ASPELL_CFLAGS="-I$aspell_includes"
     ASPELL_LIBS="-L$aspell_libs -laspell"
     AC_DEFINE_UNQUOTED(ASPELL_PATH, "${aspell_path}/", Define ASPELL's default directory)
     ifelse([$2], , :, [$2])
  else   
     AC_PATH_PROG(ASPELL, aspell, no)
     min_aspell_version=ifelse([$1], ,.50,$1)
     AC_MSG_CHECKING(for GNU/aspell - version >= $min_aspell_version)
     no_aspell=""
     if test "$ASPELL" = "no" ; then
        echo "*** The aspell executable could not be found"
        echo "*** If aspell was installed in PREFIX, make sure PREFIX/bin is in"
        echo "*** your path, or set the ASPELL environment variable to the"
        echo "*** full path to aspell or configure with --with-aspell-prefix=PREFIX."
        ASPELL_CFLAGS=""
        ASPELL_LIBS=""
        ASPELL_PATH=""
        no_aspell=yes
        ifelse([$3], , :, [$3])
     else
        ASPELL_CFLAGS="-I$aspell_includes"
        ASPELL_LIBS="-L$aspell_libs -laspell"
        aspell_version=`$ASPELL version|sed -e "s/\(@(#) International Ispell Version 3.1.20 (but really Aspell \)\(.*\))/\2/"`
        rm -f conf.aspelltest
        AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int
main ()
{
 system ("touch conf.aspelltest");
 if(strcmp("$aspell_version","$min_aspell_version")<0){
   return 1;
   }
 return 0;
}
        ],, no_aspell=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
        rm -f conf.aspelltest
	if test "x$no_aspell" = x ; then
           AC_MSG_RESULT(yes)
           AC_MSG_CHECKING(for GNU/aspell dictionaries location)
           aspell_path="`$ASPELL config dict-dir`"
           AC_DEFINE_UNQUOTED(ASPELL_PATH, "${aspell_path}/", Define ASPELL's default directory)
           AC_MSG_RESULT($aspell_path)
           AC_MSG_CHECKING(if GNU/aspell is correctly installed)
           ac_save_CFLAGS="$CFLAGS"
           ac_save_LIBS="$LIBS"
           CFLAGS="$CFLAGS $ASPELL_CFLAGS"
           LIBS="$LIBS $ASPELL_LIBS"
           AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <aspell.h>

int
main()
{
 AspellConfig * aspellconfig = new_aspell_config();
 return 0;
}
           ],, aspell_failure=yes,)
	   rm -f conftest.c
	   CFLAGS="$ac_save_CFLAGS"
           LIBS="$ac_save_LIBS"
           if test "x$aspell_failure" = x ; then     
              AC_MSG_RESULT(yes)
              ifelse([$2], , :, [$2])
           else
	      AC_MSG_RESULT(no)
	      echo "*** The GNU/aspell test program did not succeed. This usually means that "
	      echo "*** the headers and the libraries could not be found. Check config.log"
	      echo "*** for detailed error message and add the relevant options"
	      echo "*** --with-aspell-prefix, --with-aspell-includes or --with-aspell-libs"
	      echo "*** to the configure command."
	      echo "*** This can also mean that the library was not found at runtime. In that case"
	      echo "*** add its path to LD_LIBRARY_PATH environment variable or in /etc/ld.so.conf"
	      ASPELL_CFLAGS=""
	      ASPELL_LIBS=""
	      ASPELL_PATH=""
	      ifelse([$3], , :, [$3])
	   fi
	else
           AC_MSG_RESULT(no)
           ASPELL_CFLAGS=""
           ASPELL_LIBS=""
           ASPELL_PATH=""
           ifelse([$3], , :, [$3])
	fi   
     fi	
  fi
  AC_SUBST(ASPELL_CFLAGS)
  AC_SUBST(ASPELL_LIBS)
  AC_SUBST(ASPELL_PATH)
])

