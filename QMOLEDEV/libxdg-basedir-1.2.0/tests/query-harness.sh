#!/bin/sh

if [ -z "$expected" ]; then
	echo "invalid test case, missing \$expected variable" >&2
fi
if [ -z "$arguments" ]; then
	echo "invalid test case, missing \$arguments variable" >&2
fi

testquery="${top_builddir}/tests/testquery"
if [ -n "$USE_VALGRIND" ] && (type valgrind 1>/dev/null)
then
	output="`valgrind -q --error-exitcode=1 "$testquery" $arguments`"
else
	output="`"$testquery" $arguments`"
fi
runresult=$?
test x"$expected" = x"$output" -a x"$runresult" = "x0"
result=$?
if [ $# -gt 0 ] && [ x"$1" = x"-d" ]; then
	echo "for $arguments (exit $runresult)"
	echo "expected:"
	echo "$expected"
	echo "got:"
	echo "$output"
	if [ "0" = "$result" ]; then
		echo "result: pass"
	else
		echo "result: fail ($result)"
	fi
fi
exit $result
