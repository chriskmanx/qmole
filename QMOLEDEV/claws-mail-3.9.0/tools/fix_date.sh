#!/bin/sh

#  * Copyright 2004 Tristan Chabredier <wwp@claws-mail.org>
#  *
#  * This file is free software; you can redistribute it and/or modify it
#  * under the terms of the GNU General Public License as published by
#  * the Free Software Foundation; either version 3 of the License, or
#  * (at your option) any later version.
#  *
#  * This program is distributed in the hope that it will be useful, but
#  * WITHOUT ANY WARRANTY; without even the implied warranty of
#  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  * General Public License for more details.
#  *
#  * You should have received a copy of the GNU General Public License
#  * along with this program; if not, write to the Free Software
#  * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# fix_date.sh		helper script to fix non-standard date or add missing
#					date header to emails

# usage: fix_date.sh <filename> [<filename>..]
# It will replace the Date: value w/ the one picked up from more recent
# Fetchinfo time header, Received: field.. Otherwise, it will take the file
#  modification time (using a RFC 2822-compliant form).
# Any already existing X-Original-Date is kept, if missing we're adding it
# if the Date: was set (even if set w/ non conform value)

# TODO: fallback to X-OriginalArrivalTime: ?

VERSION="0.1.2"


version()
{
	echo "$VERSION"
	exit 0
}

usage()
{
	echo "usage:"
	echo "  ${0##*/} [<switches>] <filename> [<filename> ..]"
	echo "switches:"
	echo "  --help     display this help then exit"
	echo "  --version  display version information then exit"
	echo "  --force    always force (re-)writing of Date: header"
	echo "  --rfc      force re-writing of Date: header when it's not RFC-compliant"
	echo "  --debug    turn on debug information (be more verbose)"
	echo "  --strict   use RFC-strict matching patterns for dates"
	echo "  --         end of switches (in case a filename starts with a -)"
	exit $1
}

date_valid()
{
	test $STRICT -eq 1 && \
		REGEXP="$DATE_REGEXP_STRICT" || \
		REGEXP="$DATE_REGEXP"
		
	echo "$1" | grep -qEim 1 "$REGEXP"
	DATE_VALID=$?
}

dump_date_fields()
{
	test -z "$X_ORIGINAL_DATE" -a -n "$DATE" && \
		echo "X-Original-Date:$DATE" >> "$TMP"
	echo "Date:$REPLACEMENT_DATE" >> "$TMP"
}

# use --force to always (re-)write the Date header
# otherwise, the Date header will be written if only it doesn't exist
FORCE=0
# use --rfc to (re-)write the Date header when it's not RFC-compliant
# otherwise, the Date header will be written if only it doesn't exist
RFC=0
# use --debug to display more information about what's performed
DEBUG=0
# use --strict to use strict matching patterns for date validation
STRICT=0
# 0 = valid, always valid until --strict is used, then date_valid overrides this value
DATE_VALID=0

while [ -n "$1" ]
do
	case "$1" in
	--help)		usage 0;;
	--version)	version;;
	--force)	FORCE=1;;
	--debug)	DEBUG=1;;
	--rfc)		RFC=1;;
	--strict)	STRICT=1;;
	--)			shift
				break;;
	-*)			echo "error: unrecognized switch '$1'"
				usage 1;;
	*)			break;;
	esac
	shift
done

if [ $FORCE -eq 1 -a $RFC -eq 1 ]
then
	echo "error: use either --force or --rfc, but not both at the same time"
	usage 1
fi

test $# -lt 1 && \
	usage 1

TMP="/tmp/${0##*/}.tmp"
HEADERS="/tmp/${0##*/}.headers.tmp"
BODY="/tmp/${0##*/}.body.tmp"

DATE_REGEXP='( (Mon|Tue|Wed|Thu|Fri|Sat|Sun),)? [0-9]+ (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) [0-9]+ [0-9]+:[0-9]+:[0-9]+ [-+]?[0-9]+'
DATE_REGEXP_STRICT='(Mon|Tue|Wed|Thu|Fri|Sat|Sun), [0-9]+ (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) [0-9]+ [0-9]+:[0-9]+:[0-9]+ [-+]?[0-9]+'

while [ -n "$1" ]
do
	# skip if file is empty or doesn't exist
	if [ ! -s "$1" ]
	then
		shift
		continue
	fi
	SKIP=0

	# split headers and body
	# get the empty line (separation between headers and body)
	SEP=`grep -nEm1 "^$" "$1" 2>/dev/null | cut -d ':' -f 1`
	if [ -z "$SEP" -o "$SEP" = "0" ]
	then
		cp -f "$1" "$HEADERS"
		:> "$BODY"
	else
		sed -n '1,'`expr $SEP - 1`'p' "$1" > "$HEADERS"
		sed '1,'`expr $SEP - 1`'d' "$1" > "$BODY"
	fi

	# work on headers only

	# get the Date and X-Original-Date
	X_ORIGINAL_DATE=`sed -n '/^X-Original-Date:/,/^[^\t]/p' "$HEADERS" | head -n -1 | cut -d ':' -f 2-`
	DATE=`sed -n '/^Date:/,/^[^\t]/p' "$HEADERS" | head -n -1 | cut -d ':' -f 2-`

	# work on headers, minus Date and X-Original-Date
	test -n "$X_ORIGINAL_DATE" && \
		sed -i '/^X-Original-Date:/,/^[^\t]/d' "$HEADERS"
	test -n "$DATE" && \
		sed -i '/^Date:/,/^[^\t]/d' "$HEADERS"

	# found a replacement date in Fetchinfo headers
	FETCH_DATE=`grep -im1 'X-FETCH-TIME: ' "$HEADERS" | cut -d ' ' -f 2-`
	
	# or in Received: headers ..
	test $STRICT -eq 1 && \
		REGEXP="$DATE_REGEXP" || \
		REGEXP="$DATE_REGEXP_STRICT"
	RECEIVED_DATE=`sed -n '/^Received:/,/^[^\t]/p' "$HEADERS" | head -n -1 | grep -Eoim 1 "$REGEXP"`

	# .. or from FS
	FILE_DATE=`LC_ALL=POSIX LANG=POSIX ls -l --time-style="+%a, %d %b %Y %X %z" "$1" | tr -s ' ' | cut -d ' ' -f 6-11`
	# we could also use the system date as a possible replacement
	SYSTEM_DATE="`date -R`"

	# determine which replacement date to use
	if [ -z "$FETCH_DATE" ]
	then
		if [ -z "$RECEIVED_DATE" ]
		then
			# don't forget the leading whitespace here
			REPLACEMENT_DATE=" $FILE_DATE"
			REPLACEMENT="file date"
#			REPLACEMENT_DATE=" $SYSTEM_DATE"
#			REPLACEMENT="system date"
		else
			REPLACEMENT_DATE="$RECEIVED_DATE"
			REPLACEMENT="received date"
		fi
	else
		# don't forget the leading whitespace here
		REPLACEMENT_DATE=" $FETCH_DATE"
		REPLACEMENT="Fetchinfo time header"
	fi

	# ensure that the original X-Original-Date is kept
	:> "$TMP"
	if [ -n "$X_ORIGINAL_DATE" ]
	then
		echo "X-Original-Date:$X_ORIGINAL_DATE" >> "$TMP"
	fi

	# replace/set the date and write all lines
	test $RFC -eq 1 && \
		date_valid "$DATE"
	if [ -z "$DATE" ]
	then
		test $DEBUG -eq 1 && \
			echo "$1: date not found, using $REPLACEMENT now"
		dump_date_fields
	else
		if [ $FORCE -eq 1 ]
		then
			test $DEBUG -eq 1 && \
				echo "$1: date already found, replacing with $REPLACEMENT"
			dump_date_fields
		else
			if [ $RFC -eq 1 ]
			then
				if [ $DATE_VALID -ne 0 ]
				then
					test $DEBUG -eq 1 && \
						echo "$1: date already found but not RFC-compliant, replacing with $REPLACEMENT"
					dump_date_fields
				else
					test $DEBUG -eq 1 && \
						echo "$1: date already found and RFC-compliant, skipping"
					SKIP=1
				fi
			else
				test $DEBUG -eq 1 && \
					echo "$1: date already found, skipping"
				SKIP=1
			fi
		fi
	fi

	if [ $SKIP -eq 0 ]
	then
		# uncomment the following line to backup the original file
		#mv -f "$1" "$1.bak"

		cat "$HEADERS" >> "$TMP"
		cat "$BODY" >> "$TMP"
		mv -f "$TMP" "$1"
		if [ $? -ne 0 ]
		then
			echo "error while moving '$TMP' to '$1'"
			exit 1
		fi
	fi
	rm -f "$HEADERS" "$BODY" "$TMP" >/dev/null 2>&1

	shift
done
exit 0
