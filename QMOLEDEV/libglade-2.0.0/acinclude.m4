dnl JH_PYTHON_CHECK(VERSIONTUPLE, EXTRA-CHECKS,
dnl                 ACTION-IF-FOUND,ACTION-IF-NOT-FOUND)

AC_DEFUN([JH_PYTHON_CHECK],
[AC_CACHE_VAL([jh_cv_path_python],
[if test "x$PYTHON" != x; then
  jh_cv_path_python="$PYTHON"
else
dnl python code to check for required python
jh_python_check='
import sys, string
if sys.version_info < $1:
    sys.exit(1)
$2
sys.exit(0)'
dnl
  for jh_python in python python2 python2.3 python2.2 python2.1 python2.0; do
    jh_save_IFS=$IFS; IFS=$PATH_SEPARATOR
    jh_dummy="$PATH"
    for jh_dir in $jh_dummy; do
      IFS=$jh_save_IFS
      test -z "$jh_dir" && jh_dir=.
      if test -x "$jh_dir/$jh_python"; then
        echo "checking $jh_dir/$jh_python" >&AS_MESSAGE_LOG_FD
        if "$jh_dir/$jh_python" -c "$jh_python_check" 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD
        then
          echo "$jh_dir/$jh_python looks okay" >&AS_MESSAGE_LOG_FD
          jh_cv_path_python="$jh_dir/$jh_python"
          break
        fi
      fi
    done
    test -n "$jh_cv_path_python" && break
  done
fi])
if test -n "$jh_cv_path_python"; then
  PYTHON="$jh_cv_path_python"
  $3
else
  PYTHON="/usr/bin/env python"
  $4
fi
])
