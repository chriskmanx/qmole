#!/usr/bin/env bash

#  * Copyright 2007 Tristan Chabredier <wwp@claws-mail.org>
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
# popfile-link.sh		helper script to open messages in POPFile's control center
#						in order to edit their status

# Requires that POPFile is running and that the message has been processed
# by it (X-POPFile-Link: header present). POPFile control center opens with
# the web browser set in Claws Mail prefs.


open_page()
{
	TMPCMD=$(echo $OPEN_CMD | sed "s|\"%s\"|$1|")
	$TMPCMD &
}


SESSION_ID=""
if [ "$1" = "--ask-session-id" ]
then
	shift
	SESSION_ID=$(gxmessage -entry -center -wrap -buttons "OK:0,Cancel:1" -default "OK" \
		-name "popfile-link" -title "POPFile session ID" "Type in the ID of a running POPFile session to use")
	test -z "$SESSION_ID" -o $? -ne 0 && \
		exit 0
fi

test -z "$1" && \
	exit 1

CM_DIR=$(claws-mail --config-dir)
test -z "$CM_DIR" -o ! -d "$HOME/$CM_DIR" && \
	exit 1

OPEN_CMD=$(grep -Em 1 "^uri_open_command=" "$HOME/$CM_DIR/clawsrc" | cut -d '=' -f 2-)
test -z "$OPEN_CMD" && \
	exit 1

while [ -n "$1" ]
do
	LINK=$(grep -Eim 1 "^X\-POPFile\-Link: " "$1")
	if [ -n "$LINK" ]
	then
		LINK=${LINK:16}
		if [ -n "$SESSION_ID" ]
		then
			open_page "${LINK}\\&session=$SESSION_ID"
		else
			open_page "$LINK"
		fi
	fi
	shift
done
exit 0
