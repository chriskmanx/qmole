#!/bin/sh
# this script written by daniel stone <daniel@freedesktop.org>, placed in the
# public domain.

test "x$1" = "x" || . "$1"

echo "# this is a generated file -- do not edit."
echo 
echo "CURSORFILES = ${CURSORS}"
echo "CLEANFILES = \$(CURSORFILES)"
echo "cursor_DATA = \$(CURSORFILES)"
echo

for i in $CURSORS; do
	echo -n "${i}:"
	for j in 16 24 32 48 64; do
		png="${i}-${j}.png"
		EXTRA_DIST="${EXTRA_DIST} ${png}"
		echo -n " \$(srcdir)/${png}"
	done
	echo
	echo "	\$(XCURSORGEN) -p \$(srcdir) \$(srcdir)/${i}.cfg ${i}"
	echo
	EXTRA_DIST="${EXTRA_DIST} ${i}.cfg ${i}.xcf"
done

test "x$DIST" = "x" || EXTRA_DIST="${EXTRA_DIST} ${DIST}"

# the lack of space is intentional.
echo "EXTRA_DIST =${EXTRA_DIST}"
