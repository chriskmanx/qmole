#!/bin/sh
#
# Utility script to merge an xml snippet from one file into a document.
#
# To insert the file foo.inc into bar.xml,
# after the first line containing the marker <!--foo--> enter
#
# xml_insert.sh bar.xml foo foo.inc
#
# 2005 © Øyvind Kolås
#
# FIXME: add argument checking / error handling

: ${AWK="awk"}
: ${ECHO="echo"}
: ${MKDIR="mkdir"}
: ${SED="sed"}
: ${Xsed="$SED -e 1s/^X//"}

# Global variables:
EXIT_SUCCESS=0
EXIT_FAILURE=1

dirname="s,/[[^/]]*$,,"
basename="s,^.*/,,"

# Work around backward compatibility issue on IRIX 6.5. On IRIX 6.4+, sh
# is ksh but when the shell is invoked as "sh" and the current value of
# the _XPG environment variable is not equal to 1 (one), the special
# positional parameter $0, within a function call, is the name of the
# function.
progpath="$0"

# The name of this program:
# In the unlikely event $progname began with a '-', it would play havoc with
# func_echo (imagine progname=-n), so we prepend ./ in that case:
progname=`$ECHO "X$progpath" | $Xsed -e "$basename" -e 's,^-,./-,'`

# func_error arg...
# Echo program name prefixed message to standard error.
func_error ()
{
    $ECHO "$progname: "${1+"$@"} 1>&2
}

# func_fatal_error arg...
# Echo program name prefixed message to standard error, and exit.
func_fatal_error ()
{
    func_error ${1+"$@"}
    exit $EXIT_FAILURE
}

# func_mktempdir [string]
# Make a temporary directory that won't clash with other running
# processes, and avoids race conditions if possible.  If
# given, STRING is the basename for that directory.
func_mktempdir ()
{
    my_template="${TMPDIR-/tmp}/${1-$progname}"

    if test "$opt_dry_run" = ":"; then
      # Return a directory name, but don't create it in dry-run mode
      my_tmpdir="${my_template}-$$"
    else

      # If mktemp works, use that first and foremost
      my_tmpdir=`mktemp -d "${my_template}-XXXXXXXX" 2>/dev/null`

      if test ! -d "$my_tmpdir"; then
        # Failing that, at least try and use $RANDOM to avoid a race
        my_tmpdir="${my_template}-${RANDOM-0}$$"

        save_mktempdir_umask=`umask`
        umask 0077
        $MKDIR "$my_tmpdir"
        umask $save_mktempdir_umask
      fi

      # If we're not in dry-run mode, bomb out on failure
      test -d "$my_tmpdir" || \
        func_fatal_error "cannot create temporary directory \`$my_tmpdir'"
    fi

    $ECHO "X$my_tmpdir" | $Xsed
}

tmp_dir="`func_mktempdir`"
tmp_file="$tmp_dir/one"

cp $1 $tmp_file

numlines=`wc -l $tmp_file | $AWK '{print $1;}'`
splitno=`$AWK "/<\!--$2-->/ { print NR; exit 0; }" $tmp_file`
tailno=`expr $numlines - $splitno`

head -$splitno $tmp_file > $1
cat $3 >> $1
tail -$tailno $tmp_file >> $1

rm -rf $tmp_dir

exit $?
