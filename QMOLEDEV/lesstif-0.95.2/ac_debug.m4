dnl
dnl Enable malloc checker for debugging purposes
dnl See http://dmalloc.com, INSTALL(.html) for references to this.
dnl Source code which depends on this is mostly in
dnl DebugUtil.c/.h
dnl
AC_DEFUN([LT_WITH_DMALLOC],
[AC_MSG_CHECKING(if malloc debugging is wanted)
AC_ARG_WITH(dmalloc,
[  --with-dmalloc[=path]   use dmalloc, see INSTALL(.html) for reference],
[if test "$withval" = no; then
  AC_MSG_RESULT(no)
else
dnl We overwrite the variables since we won't continue in
dnl case of an error!
dnl We modify CFLAGS, and also link libs (LDFLAGS) and programs (LIBS)
  if test "$withval" != yes; then
dnl  a path was given
     CFLAGS="$CFLAGS -I$withval/include -DDMALLOC_FUNC_CHECK"

     LDFLAGS="$LDFLAGS -L$withval/lib -ldmalloc"
     LIBS="$LIBS -L$withval/lib -ldmalloc"
  else
dnl  no path was given
     CFLAGS="$CFLAGS -DDMALLOC_FUNC_CHECK"
     LDFLAGS="$LDFLAGS -ldmalloc"
     LIBS="$LIBS -ldmalloc"
  fi
  AC_TRY_LINK(
  [#include <dmalloc.h>],
  [char *ptr;
  ptr=malloc(1);
  free(ptr);
  ],
  [AC_DEFINE(WITH_DMALLOC,1,
            [Define if using the dmalloc debugging malloc package])
  AC_MSG_RESULT(Using dmalloc)],
  AC_MSG_ERROR(dmalloc not found)
  )
fi],
[AC_MSG_RESULT(no)])
])


dnl
dnl Enable another malloc checker for debugging purposes
dnl Source code which depends on this is mostly in
dnl DebugUtil.c/.h
dnl
AC_DEFUN([LT_WITH_DBMALLOC],
[AC_MSG_CHECKING(if malloc debugging is wanted)
AC_ARG_WITH(dbmalloc,
[  --with-dbmalloc[=path]   use dbmalloc, see INSTALL(.html) for reference],
[if test "$withval" = no; then
  AC_MSG_RESULT(no)
else
dnl We overwrite the variables since we won't continue in
dnl case of an error!
dnl We modify CFLAGS, and also link libs (LDFLAGS) and programs (LIBS)
  if test "$withval" != yes; then
dnl  a path was given
     CFLAGS="$CFLAGS -I$withval/include"

     LDFLAGS="$LDFLAGS -L$withval/lib -ldbmalloc"
     LIBS="$LIBS -L$withval/lib -ldbmalloc"
  else
dnl  no path was given
     LDFLAGS="$LDFLAGS -ldbmalloc"
     LIBS="$LIBS -ldbmalloc"
  fi
  AC_TRY_LINK(
  [#include <dbmalloc.h>],
  [char *ptr;
  ptr=malloc(1);
  free(ptr);
  ],
  [AC_DEFINE(WITH_DBMALLOC,1,
            [Define if using the dbmalloc debugging malloc package])
  AC_MSG_RESULT(Using dbmalloc)],
  AC_MSG_ERROR(dbmalloc not found)
  )
fi],
[AC_MSG_RESULT(no)])
])
