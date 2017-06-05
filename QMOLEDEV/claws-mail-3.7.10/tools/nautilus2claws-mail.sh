#!/usr/bin/env bash

# nautilus2claws-mail.sh
# Copyright 2004 Reza Pakdel <hrpakdel@cpsc.ucalgary.ca>

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# This script will recursively attach a number of selected 
# files/directories from Nautilus to a new blank e-mail.

# Authors: Reza Pakdel <hrpakdel@cpsc.ucalgary.ca>
#		   Stephan Sachse <white@teg-clan.de>
#
# Fixes:
#		Stephan Sachse  :  files/directorys with whitspaces


SELECTED_PATHS="${NAUTILUS_SCRIPT_SELECTED_FILE_PATHS}"
NB_SELECTED_PATHS=`echo -n "${SELECTED_PATHS}" | wc -l | awk '{print $1}'`

ATTACHMENTS=""

for ((i=${NB_SELECTED_PATHS}; i>0; i--)) ; do
	CURRENT_PATH=`echo -n "${SELECTED_PATHS}" | head -n${i} | tail -n1`
	if test -d "${CURRENT_PATH}" ; then
		FILES_FOUND=`find "${CURRENT_PATH}" -type f`
		NB_FILES_FOUND=`echo "${FILES_FOUND}" | wc -l | awk '{print $1}'`
		for ((j=${NB_FILES_FOUND}; j>0; j--)) ; do
			CURRENT_FILE=`echo "${FILES_FOUND}" | head -n${j} | tail -n1`
			ATTACHMENTS="${ATTACHMENTS} \"${CURRENT_FILE}\""
		done
	else
		ATTACHMENTS="${ATTACHMENTS} \"${CURRENT_PATH}\""
	fi
done

echo "-----------"
echo ${ATTACHMENTS}

eval "claws-mail --compose --attach ${ATTACHMENTS}"

