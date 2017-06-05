AC_DEFUN(AM_PATH_LINC,
[dnl
dnl Get the cflags and libs from the linc-config script
dnl
AC_ARG_WITH(linc-prefix,[  --with-linc-prefix=PFX   Prefix where linc is installed  (optional)],
            linc_config_prefix="$withval", linc_config_prefix="")
AC_ARG_WITH(linc-exec-prefix,[  --with-linc-exec-prefix=PFX  Exec prefix where linc is installed  (optional)],
            linc_config_exec_prefix="$withval", linc_config_exec_prefix="")
  if test x$linc_config_exec_prefix != x ; then
    linc_config_args="$linc_config_args --exec-prefix=$linc_config_exec_prefix"
    if test x${LINC_CONFIG+set} != xset ; then
      LINC_CONFIG=$linc_config_exec_prefix/bin/linc-config
    fi
  fi
  if test x$linc_config_prefix != x ; then
    linc_config_args="$linc_config_args --prefix=$linc_config_prefix"
    if test x${LINC_CONFIG+set} != xset ; then
      LINC_CONFIG=$linc_config_prefix/bin/linc-config
    fi
  fi

  AC_PATH_PROG(LINC_CONFIG, linc-config, no)
  min_linc_version=ifelse([$1], , 0.1.0, $1)

  AC_MSG_CHECKING(for linc >= $min_linc_version)
  no_linc=""
  if test "$LINC_CONFIG" = "no" ; then
    no_linc=yes
  else
    LINC_CFLAGS="`$LINC_CONFIG $linc_config_args --cflags`"
    LINC_LIBS="`$LINC_CONFIG $linc_config_args --libs`"

    linc_config_major_version=`$LINC_CONFIG $linc_config_args --version | \
           sed 's/[[^0-9]]*\([[0-9]]*\)\.\([[0-9]]*\)\.\([[0-9]]*\)/\1/'`
    linc_config_minor_version=`$LINC_CONFIG $linc_config_args --version | \
           sed 's/[[^0-9]]*\([[0-9]]*\)\.\([[0-9]]*\)\.\([[0-9]]*\)/\2/'`
    linc_config_micro_version=`$LINC_CONFIG $linc_config_args --version | \
           sed 's/[[^0-9]]*\([[0-9]]*\)\.\([[0-9]]*\)\.\([[0-9]]*\)/\3/'`
    needed_major_version=`echo $min_linc_version | \
           sed 's/[[^0-9]]*\([[0-9]]*\)\.\([[0-9]]*\)\.\([[0-9]]*\)/\1/'`
    needed_minor_version=`echo $min_linc_version | \
           sed 's/[[^0-9]]*\([[0-9]]*\)\.\([[0-9]]*\)\.\([[0-9]]*\)/\2/'`
    needed_micro_version=`echo $min_linc_version | \
           sed 's/[[^0-9]]*\([[0-9]]*\)\.\([[0-9]]*\)\.\([[0-9]]*\)/\3/'`

    if test $linc_config_major_version -lt $needed_major_version; then
        ifelse([$3], , :, [$3])
        no_linc=yes
    elif test $linc_config_major_version = $needed_major_version; then
        if test -n "$needed_minor_version" -a $linc_config_minor_version -lt $needed_minor_version; then
                ifelse([$3], , :, [$3])
                no_linc=yes
        elif test -n "$needed_minor_version" -a $linc_config_minor_version = $needed_minor_version; then
                if test -n "$needed_micro_version" -a $linc_config_micro_version -lt $needed_micro_version; then
                        ifelse([$3], , :, [$3])
                        no_linc=yes
                fi
        fi
    fi
  fi

  AC_SUBST(LINC_CFLAGS)
  AC_SUBST(LINC_LIBS)

  if test "x$no_linc" = x ; then
    AC_MSG_RESULT(yes)
    ifelse([$2], , :, [$2])
  else
    AC_MSG_RESULT(no)
    if test "$LINC_CONFIG" = "no" ; then
      echo "*** The linc-config script could not be found.  You"
      echo "*** must install the linc package, located in"
      echo "*** GNOME's cvs under the module 'linc'."
    else
      :
    fi
    LINC_CFLAGS=""
    LINC_LIBS=""
    ifelse([$3], , :, [$3])
  fi
])
