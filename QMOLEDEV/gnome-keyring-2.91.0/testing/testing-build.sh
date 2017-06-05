#!/bin/sh -eu

set -eu

# --------------------------------------------------------------------
# FUNCTIONS

usage()
{
	echo "usage: testing-build.sh -b base-name files.c ..." >&2
	exit 2
}

# --------------------------------------------------------------------
# SOURCE FILE

file_to_name()
{
	echo -n $1 | sed -E -e 's/^(unit-)?test-//' -e 's/\.c//' | tr -c '[a-z0-9_]' '_'
}

build_header()
{
	echo "/* This is auto-generated code. Edit at your own peril. */"
	echo "#include \"testing/testing.h\""
	echo

	for file in $@; do
		sed -ne "s/.*DEFINE_SETUP[ 	]*(\([^)]\+\))/DECLARE_SETUP (\1);/p" ${file}
		sed -ne "s/.*DEFINE_TEARDOWN[ 	]*(\([^)]\+\))/DECLARE_TEARDOWN (\1);/p" ${file}
		sed -ne "s/.*DEFINE_TEST[ 	]*(\([^)]\+\))/DECLARE_TEST (\1);/p" ${file}
		sed -ne "s/.*DEFINE_START[ 	]*(\([^)]\+\))/DECLARE_START (\1);/p" ${file}
		sed -ne "s/.*DEFINE_STOP[ 	]*(\([^)]\+\))/DECLARE_STOP (\1);/p" ${file}
		sed -ne "s/.*DEFINE_EXTERNAL[ 	]*(\([^)]\+\))/DECLARE_EXTERNAL (\1);/p" ${file}
	done
	echo
}

build_source()
{
	echo '/* This is auto-generated code. Edit at your own peril. */'
	echo "#include \"testing/testing.h\""
	echo "#include \"$BASE.h\""
	echo

	# Startup function
	echo "static void start_tests (void) {"
		for file in $@; do
			name=`file_to_name ${file}`
			sed -ne "s/.*DEFINE_START[ 	]*(\([^)]\+\)).*/	start_\1 ();/p" ${file}
		done
	echo "}"
	echo

	# Shutdown function
	echo "static void stop_tests (void) {"
		for file in $@; do
			name=`file_to_name ${file}`
			sed -ne "s/.*DEFINE_STOP[ 	]*(\([^)]\+\)).*/	stop_\1 ();/p" ${file}
		done
	echo "}"
	echo

	echo "static void initialize_tests (void) {"
	# Include each file, and build a test case for it
	tcases=""
	for file in $@; do
		name=`file_to_name ${file}`

		# Calculate what our setup and teardowns are.
		setup=`sed -ne "s/.*DEFINE_SETUP[ 	]*(\([^)]\+\)).*/setup_\1/p" ${file} || echo "NULL"`
		if [ -z "${setup}" ]; then
			setup="NULL"
		fi

		teardown=`sed -ne "s/.*DEFINE_TEARDOWN[ 	]*(\([^)]\+\)).*/teardown_\1/p" ${file}`
		if [ -z "${teardown}" ]; then
			teardown="NULL"
		fi

		# Add all tests to the test case
		sed -ne "s/.*DEFINE_TEST[ 	]*(\([^)]\+\)).*/	g_test_add(\"\/${name}\/\1\", int, NULL, ${setup}, test_\1, ${teardown});/p" ${file}

	done
	echo "}"
	echo

	# External function
	echo "static void run_externals (int *ret) {"
	for file in $@; do
		name=`file_to_name ${file}`
		sed -ne "s/.*DEFINE_EXTERNAL[ 	]*(\([^)]\+\)).*/	testing_external_run (\"\1\", external_\1, ret);/p" ${file}
	done
	echo "}"
	echo

	echo "static int run(void) {"
	echo "	int ret;"
	echo "	initialize_tests ();"
	echo "	start_tests ();"
	echo "	ret = g_test_run ();"
	echo "	if (ret == 0)"
	echo "		run_externals (&ret);"
	echo "	stop_tests();"
	echo "	return ret;"
	echo "}"

	echo "#include \"testing/testing.c\""
}

# --------------------------------------------------------------------
# ARGUMENT PARSING

BASE=unit

while [ $# -gt 0 ]; do
	case "$1" in
	-b)
		BASE="$2"
		shift
		;;
	--)
		shift
		break
		;;
	-*)
		usage
		;;
	*)
		break
		;;
	esac
	shift
done

build_header $* > $BASE.h
build_source $* > $BASE.c
