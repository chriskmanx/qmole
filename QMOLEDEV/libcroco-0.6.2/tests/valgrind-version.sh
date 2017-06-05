#! /bin/sh

valgrind=`which valgrind`
awk=`which awk`
if test "x$valgrind" = "x" ; then
    echo "valgrind-not-present" ;
    exit ;
fi


valgrind_version=`$valgrind --version`
if test "x$valgrind_version" = x ; then
    echo "not-present" ;
    exit
fi

if test "x$awk" = x ; then
    echo "awk-not-present"
    exit
fi

string_version=`echo $valgrind_version | $awk -F '-' '{print $2}'`

if test "x$string_version" = "x" ; then
    echo "valgrind-version-unknown"
    exit
fi

major=`echo $string_version | $awk -F '.' '{print $1}'`
minor=`echo $string_version | $awk -F '.' '{print $2}'`
micro=`echo $string_version | $awk -F '.' '{print $3}'`

version=`expr $major \* 10000 + $minor \* 100 + $micro`

if test "x$version" = "x" ; then
    echo "valgrind-version-unknown"
    exit ;
fi

if test "$version" -ge "20101" ; then
    echo "okay"
    exit ;
else
    echo "valgrind-version-lower"
    exit ;
fi


