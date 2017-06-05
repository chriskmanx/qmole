#!/bin/sh

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
# mairix.sh		mairix wrapper for Claws Mail

# if any param is passed, $1 must be the mairixrc file to use
# if no param is passed, ~/.mairix is assumed

read TEXT
test -z "$TEXT" && \
	exit 0

if [ $# -ge 1 ]
then
	RCFILE="$1"
	shift
else
	RCFILE=~/.mairixrc
fi

mairix -f "$RCFILE" --purge && \
	mairix -f "$RCFILE" "$@" $TEXT
exit $?
